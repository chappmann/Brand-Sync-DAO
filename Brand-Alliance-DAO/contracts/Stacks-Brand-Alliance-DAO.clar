;; BrandSync Collective DAO Smart Contract
;; A decentralized autonomous organization for managing brand collaborations,
;; influencer partnerships, and community-driven marketing campaigns with
;; transparent voting, reputation management, and automated fund distribution

;; ERROR CONSTANTS - All errors use ERR- prefix with descriptive kebab-case naming
(define-constant ERR-UNAUTHORIZED-ACCESS (err u100))
(define-constant ERR-COLLABORATION-PROPOSAL-NOT-FOUND (err u101))
(define-constant ERR-MEMBER-ALREADY-CAST-VOTE (err u102))
(define-constant ERR-VOTING-PERIOD-HAS-ENDED (err u103))
(define-constant ERR-VOTING-STILL-IN-PROGRESS (err u104))
(define-constant ERR-TREASURY-INSUFFICIENT-BALANCE (err u105))
(define-constant ERR-INVALID-FUNDING-AMOUNT (err u106))
(define-constant ERR-USER-NOT-DAO-MEMBER (err u107))
(define-constant ERR-USER-ALREADY-DAO-MEMBER (err u108))
(define-constant ERR-COLLABORATION-PROPOSAL-REJECTED (err u109))
(define-constant ERR-INVALID-VOTING-DURATION (err u110))
(define-constant ERR-INVALID-STRING-INPUT (err u111))

;; GOVERNANCE AND VALIDATION CONSTANTS
(define-constant dao-contract-owner tx-sender)
(define-constant minimum-voting-blocks u144) ;; ~24 hours in blocks
(define-constant maximum-voting-blocks u1008) ;; ~1 week in blocks
(define-constant required-quorum-percentage u30) ;; 30% member participation required
(define-constant winning-majority-threshold u51) ;; 51% approval needed
(define-constant initial-member-reputation-score u100)
(define-constant default-member-voting-weight u1)
(define-constant successful-proposal-reputation-bonus u10)

;; PROPOSAL STATUS ENUMERATION
(define-constant proposal-status-active u1)
(define-constant proposal-status-approved u2)
(define-constant proposal-status-rejected u3)
(define-constant proposal-status-funds-distributed u4)

;; GLOBAL STATE VARIABLES
(define-data-var next-collaboration-proposal-id uint u0)
(define-data-var total-active-dao-members uint u0)
(define-data-var collective-treasury-balance uint u0)

;; CORE DATA STRUCTURES
;; Individual DAO member profile and voting credentials
(define-map dao-member-profiles
    principal
    {
        membership-start-block: uint,
        current-voting-weight: uint,
        accumulated-reputation-score: uint,
        membership-status-active: bool
    }
)

;; Brand collaboration proposals with comprehensive details
(define-map collaboration-proposals
    uint
    {
        proposal-creator: principal,
        collaboration-title: (string-ascii 100),
        detailed-description: (string-ascii 500),
        target-brand-name: (string-ascii 50),
        requested-funding-amount: uint,
        partnership-category: (string-ascii 30),
        voting-start-block: uint,
        voting-end-block: uint,
        total-approval-votes: uint,
        total-rejection-votes: uint,
        current-proposal-status: uint,
        funds-already-distributed: bool
    }
)

;; Individual vote tracking for each proposal and member
(define-map member-proposal-votes
    {collaboration-proposal-id: uint, voting-member: principal}
    {member-vote-choice: bool, vote-cast-at-block: uint}
)

;; Brand partnership analytics and performance tracking
(define-map brand-collaboration-analytics
    (string-ascii 50)
    {
        total-partnership-proposals: uint,
        cumulative-funding-received: uint,
        successful-collaborations-count: uint,
        brand-blacklist-status: bool
    }
)

;; STRING VALIDATION HELPER FUNCTIONS
;; Validate collaboration title input
(define-private (validate-collaboration-title (title (string-ascii 100)))
    (let ((title-length (len title)))
        (and (> title-length u0) (<= title-length u100))
    )
)

;; Validate detailed description input
(define-private (validate-detailed-description (description (string-ascii 500)))
    (let ((desc-length (len description)))
        (and (> desc-length u0) (<= desc-length u500))
    )
)

;; Validate brand name input
(define-private (validate-brand-name (brand-name (string-ascii 50)))
    (let ((name-length (len brand-name)))
        (and (> name-length u0) (<= name-length u50))
    )
)

;; Validate partnership category input
(define-private (validate-partnership-category (category (string-ascii 30)))
    (let ((category-length (len category)))
        (and (> category-length u0) (<= category-length u30))
    )
)

;; MEMBERSHIP MANAGEMENT FUNCTIONS
;; Allow new users to join the DAO collective
(define-public (join-brandsync-collective)
    (let
        ((new-member-address tx-sender))
        (asserts! (is-none (map-get? dao-member-profiles new-member-address)) ERR-USER-ALREADY-DAO-MEMBER)
        
        (map-set dao-member-profiles new-member-address
            {
                membership-start-block: stacks-block-height,
                current-voting-weight: default-member-voting-weight,
                accumulated-reputation-score: initial-member-reputation-score,
                membership-status-active: true
            }
        )
        
        (var-set total-active-dao-members (+ (var-get total-active-dao-members) u1))
        (ok true)
    )
)

;; Allow members to voluntarily exit the DAO
(define-public (exit-brandsync-collective)
    (let
        ((departing-member-address tx-sender)
         (current-member-profile (unwrap! (map-get? dao-member-profiles departing-member-address) ERR-USER-NOT-DAO-MEMBER)))
        
        (asserts! (get membership-status-active current-member-profile) ERR-USER-NOT-DAO-MEMBER)
        
        (map-set dao-member-profiles departing-member-address
            (merge current-member-profile {membership-status-active: false})
        )
        
        (var-set total-active-dao-members (- (var-get total-active-dao-members) u1))
        (ok true)
    )
)

;; COLLABORATION PROPOSAL MANAGEMENT
;; Create new brand collaboration proposal
(define-public (submit-collaboration-proposal 
    (collaboration-title (string-ascii 100))
    (detailed-description (string-ascii 500))
    (target-brand-name (string-ascii 50))
    (requested-funding-amount uint)
    (partnership-category (string-ascii 30))
    (voting-duration-blocks uint))
    
    (let
        ((proposal-creator-address tx-sender)
         (new-proposal-id (+ (var-get next-collaboration-proposal-id) u1))
         (creator-member-profile (unwrap! (map-get? dao-member-profiles proposal-creator-address) ERR-USER-NOT-DAO-MEMBER))
         (calculated-voting-end-block (+ stacks-block-height voting-duration-blocks)))
        
        ;; Comprehensive input validation
        (asserts! (get membership-status-active creator-member-profile) ERR-USER-NOT-DAO-MEMBER)
        (asserts! (> requested-funding-amount u0) ERR-INVALID-FUNDING-AMOUNT)
        (asserts! (>= voting-duration-blocks minimum-voting-blocks) ERR-INVALID-VOTING-DURATION)
        (asserts! (<= voting-duration-blocks maximum-voting-blocks) ERR-INVALID-VOTING-DURATION)
        
        ;; String input validation
        (asserts! (validate-collaboration-title collaboration-title) ERR-INVALID-STRING-INPUT)
        (asserts! (validate-detailed-description detailed-description) ERR-INVALID-STRING-INPUT)
        (asserts! (validate-brand-name target-brand-name) ERR-INVALID-STRING-INPUT)
        (asserts! (validate-partnership-category partnership-category) ERR-INVALID-STRING-INPUT)
        
        ;; Store new collaboration proposal
        (map-set collaboration-proposals new-proposal-id
            {
                proposal-creator: proposal-creator-address,
                collaboration-title: collaboration-title,
                detailed-description: detailed-description,
                target-brand-name: target-brand-name,
                requested-funding-amount: requested-funding-amount,
                partnership-category: partnership-category,
                voting-start-block: stacks-block-height,
                voting-end-block: calculated-voting-end-block,
                total-approval-votes: u0,
                total-rejection-votes: u0,
                current-proposal-status: proposal-status-active,
                funds-already-distributed: false
            }
        )
        
        ;; Update global proposal counter
        (var-set next-collaboration-proposal-id new-proposal-id)
        
        ;; Initialize brand analytics if needed
        (initialize-brand-analytics-if-needed target-brand-name)
        
        (ok new-proposal-id)
    )
)

;; Cast vote on collaboration proposal
(define-public (cast-collaboration-vote (collaboration-proposal-id uint) (approve-proposal bool))
    (let
        ((voting-member-address tx-sender)
         (target-proposal (unwrap! (map-get? collaboration-proposals collaboration-proposal-id) ERR-COLLABORATION-PROPOSAL-NOT-FOUND))
         (voter-member-profile (unwrap! (map-get? dao-member-profiles voting-member-address) ERR-USER-NOT-DAO-MEMBER))
         (existing-vote-check (map-get? member-proposal-votes {collaboration-proposal-id: collaboration-proposal-id, voting-member: voting-member-address})))
        
        ;; Validate voting eligibility and timing
        (asserts! (get membership-status-active voter-member-profile) ERR-USER-NOT-DAO-MEMBER)
        (asserts! (is-none existing-vote-check) ERR-MEMBER-ALREADY-CAST-VOTE)
        (asserts! (<= stacks-block-height (get voting-end-block target-proposal)) ERR-VOTING-PERIOD-HAS-ENDED)
        (asserts! (is-eq (get current-proposal-status target-proposal) proposal-status-active) ERR-VOTING-PERIOD-HAS-ENDED)
        
        ;; Record member's vote
        (map-set member-proposal-votes {collaboration-proposal-id: collaboration-proposal-id, voting-member: voting-member-address}
            {member-vote-choice: approve-proposal, vote-cast-at-block: stacks-block-height}
        )
        
        ;; Update proposal vote tallies
        (if approve-proposal
            (map-set collaboration-proposals collaboration-proposal-id
                (merge target-proposal {total-approval-votes: (+ (get total-approval-votes target-proposal) (get current-voting-weight voter-member-profile))}))
            (map-set collaboration-proposals collaboration-proposal-id
                (merge target-proposal {total-rejection-votes: (+ (get total-rejection-votes target-proposal) (get current-voting-weight voter-member-profile))}))
        )
        
        (ok true)
    )
)

;; Execute approved collaboration proposal and distribute funds
(define-public (execute-approved-collaboration (collaboration-proposal-id uint))
    (let
        ((target-proposal (unwrap! (map-get? collaboration-proposals collaboration-proposal-id) ERR-COLLABORATION-PROPOSAL-NOT-FOUND)))
        
        ;; Ensure voting period has concluded
        (asserts! (> stacks-block-height (get voting-end-block target-proposal)) ERR-VOTING-STILL-IN-PROGRESS)
        (asserts! (not (get funds-already-distributed target-proposal)) ERR-COLLABORATION-PROPOSAL-REJECTED)
        
        ;; Calculate voting results and determine approval
        (let
            ((total-votes-cast (+ (get total-approval-votes target-proposal) (get total-rejection-votes target-proposal)))
             (required-quorum-votes (/ (* (var-get total-active-dao-members) required-quorum-percentage) u100))
             (proposal-meets-requirements (and 
                (>= total-votes-cast required-quorum-votes)
                (> (* (get total-approval-votes target-proposal) u100) (* total-votes-cast winning-majority-threshold)))))
            
            (if proposal-meets-requirements
                (begin
                    ;; Verify and transfer requested funds
                    (asserts! (>= (var-get collective-treasury-balance) (get requested-funding-amount target-proposal)) ERR-TREASURY-INSUFFICIENT-BALANCE)
                    
                    ;; Update proposal status to approved and executed
                    (map-set collaboration-proposals collaboration-proposal-id
                        (merge target-proposal {current-proposal-status: proposal-status-funds-distributed, funds-already-distributed: true}))
                    
                    ;; Deduct funds from treasury
                    (var-set collective-treasury-balance (- (var-get collective-treasury-balance) (get requested-funding-amount target-proposal)))
                    
                    ;; Update brand collaboration analytics
                    (update-brand-collaboration-metrics (get target-brand-name target-proposal) (get requested-funding-amount target-proposal) true)
                    
                    ;; Reward proposal creator with reputation boost
                    (enhance-member-reputation (get proposal-creator target-proposal) successful-proposal-reputation-bonus)
                    
                    (ok true))
                (begin
                    ;; Mark proposal as rejected
                    (map-set collaboration-proposals collaboration-proposal-id
                        (merge target-proposal {current-proposal-status: proposal-status-rejected, funds-already-distributed: true}))
                    ERR-COLLABORATION-PROPOSAL-REJECTED)
            )
        )
    )
)

;; TREASURY AND FINANCIAL MANAGEMENT
;; Add funds to the collective treasury
(define-public (contribute-to-collective-treasury (contribution-amount uint))
    (begin
        (asserts! (> contribution-amount u0) ERR-INVALID-FUNDING-AMOUNT)
        (var-set collective-treasury-balance (+ (var-get collective-treasury-balance) contribution-amount))
        (ok true)
    )
)

;; INTERNAL HELPER FUNCTIONS
;; Initialize brand analytics for new partnerships
(define-private (initialize-brand-analytics-if-needed (target-brand-name (string-ascii 50)))
    (let
        ((existing-brand-analytics (default-to 
            {total-partnership-proposals: u0, cumulative-funding-received: u0, successful-collaborations-count: u0, brand-blacklist-status: false}
            (map-get? brand-collaboration-analytics target-brand-name))))
        
        (map-set brand-collaboration-analytics target-brand-name
            {
                total-partnership-proposals: (+ (get total-partnership-proposals existing-brand-analytics) u1),
                cumulative-funding-received: (get cumulative-funding-received existing-brand-analytics),
                successful-collaborations-count: (get successful-collaborations-count existing-brand-analytics),
                brand-blacklist-status: (get brand-blacklist-status existing-brand-analytics)
            }
        )
    )
)

;; Update brand collaboration success metrics
(define-private (update-brand-collaboration-metrics (target-brand-name (string-ascii 50)) (funding-amount uint) (collaboration-successful bool))
    (let
        ((current-brand-analytics (default-to 
            {total-partnership-proposals: u0, cumulative-funding-received: u0, successful-collaborations-count: u0, brand-blacklist-status: false}
            (map-get? brand-collaboration-analytics target-brand-name))))
        
        (map-set brand-collaboration-analytics target-brand-name
            {
                total-partnership-proposals: (get total-partnership-proposals current-brand-analytics),
                cumulative-funding-received: (+ (get cumulative-funding-received current-brand-analytics) funding-amount),
                successful-collaborations-count: (if collaboration-successful 
                    (+ (get successful-collaborations-count current-brand-analytics) u1) 
                    (get successful-collaborations-count current-brand-analytics)),
                brand-blacklist-status: (get brand-blacklist-status current-brand-analytics)
            }
        )
    )
)

;; Enhance member reputation score for contributions
(define-private (enhance-member-reputation (target-member-address principal) (reputation-bonus-points uint))
    (match (map-get? dao-member-profiles target-member-address)
        current-member-profile (map-set dao-member-profiles target-member-address
            (merge current-member-profile {accumulated-reputation-score: (+ (get accumulated-reputation-score current-member-profile) reputation-bonus-points)}))
        false
    )
)

;; PUBLIC READ-ONLY QUERY FUNCTIONS
;; Retrieve specific collaboration proposal details
(define-read-only (get-collaboration-proposal-details (collaboration-proposal-id uint))
    (map-get? collaboration-proposals collaboration-proposal-id)
)

;; Retrieve DAO member profile information
(define-read-only (get-dao-member-profile (member-address principal))
    (map-get? dao-member-profiles member-address)
)

;; Retrieve brand collaboration analytics and performance
(define-read-only (get-brand-collaboration-analytics (target-brand-name (string-ascii 50)))
    (map-get? brand-collaboration-analytics target-brand-name)
)

;; Get current collective treasury balance
(define-read-only (get-collective-treasury-balance)
    (var-get collective-treasury-balance)
)

;; Get total active DAO member count
(define-read-only (get-total-active-members)
    (var-get total-active-dao-members)
)

;; Get next proposal ID that will be assigned
(define-read-only (get-next-proposal-id)
    (var-get next-collaboration-proposal-id)
)

;; Check if member has voted on specific proposal
(define-read-only (check-member-voting-status (collaboration-proposal-id uint) (member-address principal))
    (is-some (map-get? member-proposal-votes {collaboration-proposal-id: collaboration-proposal-id, voting-member: member-address}))
)

;; Retrieve member's specific vote details
(define-read-only (get-member-vote-details (collaboration-proposal-id uint) (member-address principal))
    (map-get? member-proposal-votes {collaboration-proposal-id: collaboration-proposal-id, voting-member: member-address}))

;; ADMINISTRATIVE GOVERNANCE FUNCTIONS (Owner Only)
;; Add brand to blacklist for policy violations
(define-public (blacklist-brand-partner (target-brand-name (string-ascii 50)))
    (begin
        (asserts! (is-eq tx-sender dao-contract-owner) ERR-UNAUTHORIZED-ACCESS)
        (asserts! (validate-brand-name target-brand-name) ERR-INVALID-STRING-INPUT)
        (let
            ((current-brand-analytics (default-to 
                {total-partnership-proposals: u0, cumulative-funding-received: u0, successful-collaborations-count: u0, brand-blacklist-status: false}
                (map-get? brand-collaboration-analytics target-brand-name))))
            
            (map-set brand-collaboration-analytics target-brand-name
                (merge current-brand-analytics {brand-blacklist-status: true}))
            (ok true)
        )
    )
)

;; Emergency member removal function
(define-public (remove-dao-member-emergency (target-member-address principal))
    (begin
        (asserts! (is-eq tx-sender dao-contract-owner) ERR-UNAUTHORIZED-ACCESS)
        (let
            ((target-member-profile (unwrap! (map-get? dao-member-profiles target-member-address) ERR-USER-NOT-DAO-MEMBER)))
            
            (asserts! (get membership-status-active target-member-profile) ERR-USER-NOT-DAO-MEMBER)
            (map-set dao-member-profiles target-member-address
                (merge target-member-profile {membership-status-active: false}))
            (var-set total-active-dao-members (- (var-get total-active-dao-members) u1))
            (ok true)
        )
    )
)