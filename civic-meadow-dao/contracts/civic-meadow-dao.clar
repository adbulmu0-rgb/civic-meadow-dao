;; CivicMeadow DAO 
;; Implements: quadratic voting, liquid delegation, time-weighted reputation,
;; and a Contribution Proof Protocol for decentralized governance.

;; =============================================================================
;; CONSTANTS
;; =============================================================================

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED        (err u100))
(define-constant ERR-PROPOSAL-NOT-FOUND   (err u101))
(define-constant ERR-ALREADY-VOTED        (err u102))
(define-constant ERR-PROPOSAL-CLOSED      (err u103))
(define-constant ERR-PROPOSAL-ACTIVE      (err u104))
(define-constant ERR-INSUFFICIENT-TOKENS  (err u105))
(define-constant ERR-INVALID-DELEGATE     (err u106))
(define-constant ERR-SELF-DELEGATE        (err u107))
(define-constant ERR-PROPOSAL-NOT-PASSED  (err u108))
(define-constant ERR-ALREADY-EXECUTED     (err u109))
(define-constant ERR-TOKENS-LOCKED        (err u110))

;; Voting period: ~2 days in blocks (assuming 10-min blocks)
(define-constant VOTING-PERIOD u288)

;; Reputation decay: applied per proposal cycle (in basis points, 100 = 1%)
(define-constant REPUTATION-DECAY-BPS u200)

;; Escrow lock period after proposal passes (~1 day)
(define-constant ESCROW-LOCK-PERIOD u144)

;; Quadratic voting scale factor (votes = floor(sqrt(tokens * SCALE)))
;; We approximate integer sqrt on-chain.
(define-constant QUADRATIC-SCALE u1000000)

;; =============================================================================
;; DATA VARS
;; =============================================================================

(define-data-var proposal-count uint u0)

;; =============================================================================
;; DATA MAPS
;; =============================================================================

;; Token balances (governance token ledger)
(define-map token-balances principal uint)

;; Locked tokens per user (escrowed during active proposals they voted on)
(define-map locked-tokens principal uint)

;; Reputation scores (time-weighted, decays each cycle)
(define-map reputation-scores principal uint)

;; Delegation: voter -> delegate (topic-agnostic for simplicity)
(define-map delegations principal principal)

;; Proposals
(define-map proposals
  uint
  {
    proposer: principal,
    title: (string-ascii 128),
    description: (string-ascii 512),
    start-block: uint,
    end-block: uint,
    votes-for: uint,
    votes-against: uint,
    executed: bool,
    execute-after: uint    ;; block at which escrow unlocks and execution is allowed
  }
)

;; Vote receipt: (proposal-id, voter) -> vote direction
(define-map vote-receipts
  { proposal-id: uint, voter: principal }
  { support: bool, vote-weight: uint }
)

;; Contribution scores (raw, before reputation weighting)
(define-map contribution-scores principal uint)

;; =============================================================================
;; PRIVATE HELPERS
;; =============================================================================

;; Integer square root (Babylonian method, fixed iterations for on-chain use)
;; Uses a single let with sequential bindings - each x_n depends on the prior.
(define-private (isqrt (n uint))
  (if (is-eq n u0)
    u0
    (let (
      (x1 (/ (+ n u1) u2))
      (x2 (/ (+ (/ (+ n u1) u2) (/ n (/ (+ n u1) u2))) u2))
    )
      ;; Run additional refinement passes using intermediate values
      (let (
        (x3 (/ (+ x2 (/ x1 x2)) u2))
        (x4 (/ (+ x2 (/ n x2)) u2))
      )
        (let (
          (x5 (/ (+ x4 (/ n x4)) u2))
          (x6 (/ (+ x3 (/ n x3)) u2))
        )
          (let (
            (x7 (/ (+ x5 (/ n x5)) u2))
            (x8 (/ (+ x6 (/ n x6)) u2))
          )
            ;; Return the smaller of the two converged estimates
            (if (<= x7 x8) x7 x8)
          )
        )
      )
    )
  )
)

;; Compute quadratic vote weight from token amount
;; weight = isqrt(tokens * QUADRATIC-SCALE)
(define-private (quadratic-weight (tokens uint))
  (isqrt (* tokens QUADRATIC-SCALE))
)

;; Get effective delegate (follow one level of delegation)
(define-private (get-effective-voter (voter principal))
  (match (map-get? delegations voter)
    delegate delegate
    voter
  )
)

;; Apply reputation decay to a score (returns decayed value)
(define-private (decay-reputation (score uint))
  (let ((decay (/ (* score REPUTATION-DECAY-BPS) u10000)))
    (if (> score decay) (- score decay) u0)
  )
)

;; =============================================================================
;; TOKEN MANAGEMENT (simplified internal ledger)
;; =============================================================================

;; Mint governance tokens (owner only, for bootstrapping / testing)
(define-public (mint-tokens (recipient principal) (amount uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set token-balances recipient
      (+ (default-to u0 (map-get? token-balances recipient)) amount))
    (ok true)
  )
)

;; Transfer tokens between principals
(define-public (transfer-tokens (to principal) (amount uint))
  (let (
    (sender-balance (default-to u0 (map-get? token-balances tx-sender)))
    (sender-locked  (default-to u0 (map-get? locked-tokens  tx-sender)))
    (available      (if (> sender-balance sender-locked)
                      (- sender-balance sender-locked)
                      u0))
  )
    (asserts! (>= available amount) ERR-INSUFFICIENT-TOKENS)
    (map-set token-balances tx-sender (- sender-balance amount))
    (map-set token-balances to
      (+ (default-to u0 (map-get? token-balances to)) amount))
    (ok true)
  )
)

;; =============================================================================
;; REPUTATION & CONTRIBUTIONS
;; =============================================================================

;; Award contribution points (owner only; in production this could be a trusted oracle)
(define-public (award-contribution (user principal) (points uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (let ((current (default-to u0 (map-get? contribution-scores user))))
      (map-set contribution-scores user (+ current points))
      ;; Contribution also boosts reputation
      (let ((rep (default-to u0 (map-get? reputation-scores user))))
        (map-set reputation-scores user (+ rep points))
      )
    )
    (ok true)
  )
)

;; Decay reputation for a user (callable by anyone to keep scores honest)
(define-public (apply-reputation-decay (user principal))
  (let ((rep (default-to u0 (map-get? reputation-scores user))))
    (map-set reputation-scores user (decay-reputation rep))
    (ok true)
  )
)

;; =============================================================================
;; DELEGATION (liquid democracy)
;; =============================================================================

;; Delegate voting power to another principal (topic-agnostic)
(define-public (delegate-to (delegate principal))
  (begin
    (asserts! (not (is-eq tx-sender delegate)) ERR-SELF-DELEGATE)
    (asserts! (not (is-eq delegate CONTRACT-OWNER)) ERR-INVALID-DELEGATE)
    (map-set delegations tx-sender delegate)
    (ok true)
  )
)

;; Remove delegation (revert to direct voting)
(define-public (remove-delegation)
  (begin
    (map-delete delegations tx-sender)
    (ok true)
  )
)

;; =============================================================================
;; PROPOSALS
;; =============================================================================

;; Create a new governance proposal
(define-public (create-proposal (title (string-ascii 128)) (description (string-ascii 512)))
  (let (
    (proposal-id (+ (var-get proposal-count) u1))
    (start       block-height)
    (end         (+ block-height VOTING-PERIOD))
  )
    ;; Proposer must hold at least 1 token
    (asserts!
      (>= (default-to u0 (map-get? token-balances tx-sender)) u1)
      ERR-INSUFFICIENT-TOKENS)
    (map-set proposals proposal-id {
      proposer:      tx-sender,
      title:         title,
      description:   description,
      start-block:   start,
      end-block:     end,
      votes-for:     u0,
      votes-against: u0,
      executed:      false,
      execute-after: u0
    })
    (var-set proposal-count proposal-id)
    ;; Award contribution for proposing
    (let ((rep (default-to u0 (map-get? reputation-scores tx-sender))))
      (map-set reputation-scores tx-sender (+ rep u10))
    )
    (ok proposal-id)
  )
)

;; Cast a vote (quadratic weight, with delegation support)
(define-public (cast-vote (proposal-id uint) (support bool))
  (let (
    (effective-voter (get-effective-voter tx-sender))
    (proposal        (unwrap! (map-get? proposals proposal-id) ERR-PROPOSAL-NOT-FOUND))
    (tokens          (default-to u0 (map-get? token-balances effective-voter)))
    (locked          (default-to u0 (map-get? locked-tokens  effective-voter)))
    (available-bal   (if (> tokens locked) (- tokens locked) u0))
    (weight          (quadratic-weight available-bal))
  )
    ;; Proposal must be active
    (asserts! (<= block-height (get end-block proposal)) ERR-PROPOSAL-CLOSED)
    (asserts! (>= block-height (get start-block proposal)) ERR-PROPOSAL-CLOSED)
    ;; No double voting
    (asserts!
      (is-none (map-get? vote-receipts { proposal-id: proposal-id, voter: effective-voter }))
      ERR-ALREADY-VOTED)
    (asserts! (> weight u0) ERR-INSUFFICIENT-TOKENS)

    ;; Record vote
    (map-set vote-receipts
      { proposal-id: proposal-id, voter: effective-voter }
      { support: support, vote-weight: weight })

    ;; Update tally
    (if support
      (map-set proposals proposal-id
        (merge proposal { votes-for: (+ (get votes-for proposal) weight) }))
      (map-set proposals proposal-id
        (merge proposal { votes-against: (+ (get votes-against proposal) weight) }))
    )

    ;; Lock tokens (escrow accountability) - add to any existing lock
    (map-set locked-tokens effective-voter
      (+ locked (/ available-bal u2)))  ;; lock 50% of available balance

    ;; Award reputation for participation
    (let ((rep (default-to u0 (map-get? reputation-scores effective-voter))))
      (map-set reputation-scores effective-voter (+ rep u5))
    )

    (ok weight)
  )
)

;; Execute a passed proposal (unlocks escrow, marks executed)
(define-public (execute-proposal (proposal-id uint))
  (let ((proposal (unwrap! (map-get? proposals proposal-id) ERR-PROPOSAL-NOT-FOUND)))
    ;; Voting must have ended
    (asserts! (> block-height (get end-block proposal)) ERR-PROPOSAL-ACTIVE)
    ;; Must not be already executed
    (asserts! (not (get executed proposal)) ERR-ALREADY-EXECUTED)
    ;; Proposal must have passed (more for than against)
    (asserts!
      (> (get votes-for proposal) (get votes-against proposal))
      ERR-PROPOSAL-NOT-PASSED)
    ;; Mark executed and set escrow-release block
    (map-set proposals proposal-id
      (merge proposal {
        executed:      true,
        execute-after: (+ block-height ESCROW-LOCK-PERIOD)
      }))
    (ok true)
  )
)

;; Release locked tokens after escrow period (voter calls this themselves)
(define-public (release-escrow (proposal-id uint))
  (let (
    (proposal (unwrap! (map-get? proposals proposal-id) ERR-PROPOSAL-NOT-FOUND))
    (locked   (default-to u0 (map-get? locked-tokens tx-sender)))
  )
    (asserts! (get executed proposal)                ERR-PROPOSAL-NOT-PASSED)
    (asserts! (>= block-height (get execute-after proposal)) ERR-TOKENS-LOCKED)
    ;; Release all locked tokens (simplified: full unlock post-escrow)
    (map-set locked-tokens tx-sender u0)
    (ok true)
  )
)

;; =============================================================================
;; READ-ONLY VIEWS
;; =============================================================================

;; Get proposal details
(define-read-only (get-proposal (proposal-id uint))
  (map-get? proposals proposal-id)
)

;; Get token balance for a principal
(define-read-only (get-balance (user principal))
  (default-to u0 (map-get? token-balances user))
)

;; Get locked token amount for a principal
(define-read-only (get-locked (user principal))
  (default-to u0 (map-get? locked-tokens user))
)

;; Get reputation score for a principal
(define-read-only (get-reputation (user principal))
  (default-to u0 (map-get? reputation-scores user))
)

;; Get contribution score for a principal
(define-read-only (get-contribution (user principal))
  (default-to u0 (map-get? contribution-scores user))
)

;; Get current delegate for a principal (none if voting directly)
(define-read-only (get-delegate (user principal))
  (map-get? delegations user)
)

;; Get vote receipt for a proposal and voter
(define-read-only (get-vote-receipt (proposal-id uint) (voter principal))
  (map-get? vote-receipts { proposal-id: proposal-id, voter: voter })
)

;; Get total number of proposals created
(define-read-only (get-proposal-count)
  (var-get proposal-count)
)

;; Compute the quadratic vote weight a user would have right now
(define-read-only (get-voting-power (user principal))
  (let (
    (effective (get-effective-voter user))
    (tokens    (default-to u0 (map-get? token-balances effective)))
    (locked    (default-to u0 (map-get? locked-tokens  effective)))
    (available (if (> tokens locked) (- tokens locked) u0))
  )
    (quadratic-weight available)
  )
)

;; Check whether a proposal is currently active
(define-read-only (is-proposal-active (proposal-id uint))
  (match (map-get? proposals proposal-id)
    proposal
      (and
        (>= block-height (get start-block proposal))
        (<= block-height (get end-block   proposal)))
    false
  )
)

;; Check whether a proposal passed
(define-read-only (did-proposal-pass (proposal-id uint))
  (match (map-get? proposals proposal-id)
    proposal
      (and
        (> block-height (get end-block proposal))
        (> (get votes-for proposal) (get votes-against proposal)))
    false
  )
)
