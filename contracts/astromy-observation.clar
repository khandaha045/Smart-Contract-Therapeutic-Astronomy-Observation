;; ===================================================================
;; SMART CONTRACT THERAPEUTIC ASTRONOMY OBSERVATION SYSTEM (SCTAOS)
;; ===================================================================
;; A comprehensive system for coordinating stargazing healing experiences
;; with telescope sharing, celestial event tracking, and community cultivation

;; ===================================================================
;; CONSTANTS
;; ===================================================================

(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u401))
(define-constant ERR_NOT_FOUND (err u404))
(define-constant ERR_INVALID_INPUT (err u400))
(define-constant ERR_ALREADY_EXISTS (err u409))
(define-constant ERR_INSUFFICIENT_FUNDS (err u402))
(define-constant ERR_EVENT_FULL (err u423))
(define-constant ERR_TELESCOPE_UNAVAILABLE (err u503))
(define-constant ERR_SESSION_EXPIRED (err u410))

;; Event types
(define-constant EVENT_TYPE_METEOR_SHOWER u1)
(define-constant EVENT_TYPE_LUNAR_ECLIPSE u2)
(define-constant EVENT_TYPE_SOLAR_ECLIPSE u3)
(define-constant EVENT_TYPE_PLANET_CONJUNCTION u4)
(define-constant EVENT_TYPE_COMET_VIEWING u5)
(define-constant EVENT_TYPE_DEEP_SKY u6)
(define-constant EVENT_TYPE_THERAPEUTIC_SESSION u7)

;; Accessibility types
(define-constant ACCESSIBILITY_WHEELCHAIR u1)
(define-constant ACCESSIBILITY_VISUAL_IMPAIRED u2)
(define-constant ACCESSIBILITY_HEARING_IMPAIRED u4)
(define-constant ACCESSIBILITY_COGNITIVE u8)

;; Light pollution levels (0-9 Bortle scale)
(define-constant LIGHT_POLLUTION_EXCELLENT u1)
(define-constant LIGHT_POLLUTION_TYPICAL_DARK u2)
(define-constant LIGHT_POLLUTION_RURAL u3)
(define-constant LIGHT_POLLUTION_SUBURBAN u6)
(define-constant LIGHT_POLLUTION_CITY u9)

;; ===================================================================
;; DATA VARIABLES
;; ===================================================================

(define-data-var next-event-id uint u1)
(define-data-var next-telescope-id uint u1)
(define-data-var next-session-id uint u1)
(define-data-var contract-active bool true)
(define-data-var total-therapeutic-sessions uint u0)
(define-data-var total-wonder-moments uint u0)

;; ===================================================================
;; DATA MAPS
;; ===================================================================

;; Celestial Events
(define-map celestial-events
    { event-id: uint }
    {
        event-type: uint,
        name: (string-ascii 128),
        description: (string-ascii 512),
        start-time: uint,
        end-time: uint,
        location: (string-ascii 256),
        coordinates: { lat: int, lng: int },
        light-pollution-level: uint,
        max-participants: uint,
        current-participants: uint,
        accessibility-features: uint,
        therapeutic-focus: (string-ascii 256),
        created-by: principal,
        created-at: uint,
        is-active: bool
    }
)

;; Telescopes
(define-map telescopes
    { telescope-id: uint }
    {
        owner: principal,
        model: (string-ascii 128),
        aperture-mm: uint,
        focal-length-mm: uint,
        location: (string-ascii 256),
        coordinates: { lat: int, lng: int },
        hourly-rate: uint,
        accessibility-features: uint,
        availability-start: uint,
        availability-end: uint,
        is-available: bool,
        therapeutic-certified: bool,
        total-sessions: uint,
        rating-sum: uint,
        rating-count: uint
    }
)

;; Therapeutic Sessions
(define-map therapeutic-sessions
    { session-id: uint }
    {
        participant: principal,
        event-id: uint,
        telescope-id: (optional uint),
        session-type: (string-ascii 128),
        duration-minutes: uint,
        therapeutic-goals: (string-ascii 512),
        facilitator: principal,
        start-time: uint,
        completion-status: (string-ascii 64),
        wonder-score: uint,
        cosmic-perspective-gained: bool,
        reflection-notes: (string-ascii 1024),
        created-at: uint
    }
)

;; Event Participants
(define-map event-participants
    { event-id: uint, participant: principal }
    {
        joined-at: uint,
        accessibility-needs: uint,
        therapeutic-goals: (string-ascii 256),
        experience-level: uint,
        wonder-moments: uint,
        completion-status: (string-ascii 64)
    }
)

;; Telescope Reservations
(define-map telescope-reservations
    { telescope-id: uint, start-time: uint }
    {
        reserver: principal,
        end-time: uint,
        event-id: (optional uint),
        payment-amount: uint,
        therapeutic-session: bool,
        created-at: uint,
        status: (string-ascii 32)
    }
)

;; Community Wonder Cultivation
(define-map community-members
    { member: principal }
    {
        join-date: uint,
        total-sessions: uint,
        total-wonder-moments: uint,
        cosmic-perspective-level: uint,
        accessibility-needs: uint,
        preferred-event-types: uint,
        therapeutic-journey-notes: (string-ascii 1024),
        light-pollution-advocate: bool,
        mentor-status: bool
    }
)

;; Light Pollution Advocacy
(define-map light-pollution-reports
    { report-id: uint }
    {
        reporter: principal,
        location: (string-ascii 256),
        coordinates: { lat: int, lng: int },
        pollution-level: uint,
        impact-description: (string-ascii 512),
        proposed-solutions: (string-ascii 512),
        report-date: uint,
        community-support: uint,
        resolution-status: (string-ascii 64)
    }
)

;; ===================================================================
;; PRIVATE FUNCTIONS
;; ===================================================================

(define-private (is-valid-coordinates (lat int) (lng int))
    (and
        (>= lat -90000000)  ;; -90.000000 degrees
        (<= lat 90000000)   ;; 90.000000 degrees
        (>= lng -180000000) ;; -180.000000 degrees
        (<= lng 180000000)  ;; 180.000000 degrees
    )
)

(define-private (is-valid-time-range (start uint) (end uint))
    (and
        (> start stacks-block-height)
        (> end start)
        (< (- end start) u525600) ;; Max 1 year duration
    )
)

(define-private (calculate-distance-score (lat1 int) (lng1 int) (lat2 int) (lng2 int))
    ;; Simplified distance calculation for proximity scoring
    (let ((lat-diff (if (> lat1 lat2) (- lat1 lat2) (- lat2 lat1)))
          (lng-diff (if (> lng1 lng2) (- lng1 lng2) (- lng2 lng1))))
        (+ lat-diff lng-diff)
    )
)

(define-private (update-wonder-metrics (participant principal) (wonder-gained uint))
    (let ((member-data (default-to
                        { join-date: stacks-block-height, total-sessions: u0, total-wonder-moments: u0,
                          cosmic-perspective-level: u0, accessibility-needs: u0, preferred-event-types: u0,
                          therapeutic-journey-notes: "", light-pollution-advocate: false, mentor-status: false }
                        (map-get? community-members { member: participant }))))
        (map-set community-members
            { member: participant }
            (merge member-data {
                total-wonder-moments: (+ (get total-wonder-moments member-data) wonder-gained),
                cosmic-perspective-level: (+ (get cosmic-perspective-level member-data) u1)
            })
        )
    )
)

;; ===================================================================
;; PUBLIC FUNCTIONS - EVENT MANAGEMENT
;; ===================================================================

(define-public (create-celestial-event
    (event-type uint)
    (name (string-ascii 128))
    (description (string-ascii 512))
    (start-time uint)
    (end-time uint)
    (location (string-ascii 256))
    (lat int)
    (lng int)
    (light-pollution-level uint)
    (max-participants uint)
    (accessibility-features uint)
    (therapeutic-focus (string-ascii 256))
)
    (let ((event-id (var-get next-event-id)))
        (asserts! (var-get contract-active) ERR_UNAUTHORIZED)
        (asserts! (and (> event-type u0) (<= event-type EVENT_TYPE_THERAPEUTIC_SESSION)) ERR_INVALID_INPUT)
        (asserts! (is-valid-coordinates lat lng) ERR_INVALID_INPUT)
        (asserts! (is-valid-time-range start-time end-time) ERR_INVALID_INPUT)
        (asserts! (and (>= light-pollution-level u1) (<= light-pollution-level u9)) ERR_INVALID_INPUT)
        (asserts! (> max-participants u0) ERR_INVALID_INPUT)

        (map-set celestial-events
            { event-id: event-id }
            {
                event-type: event-type,
                name: name,
                description: description,
                start-time: start-time,
                end-time: end-time,
                location: location,
                coordinates: { lat: lat, lng: lng },
                light-pollution-level: light-pollution-level,
                max-participants: max-participants,
                current-participants: u0,
                accessibility-features: accessibility-features,
                therapeutic-focus: therapeutic-focus,
                created-by: tx-sender,
                created-at: stacks-block-height,
                is-active: true
            }
        )

        (var-set next-event-id (+ event-id u1))
        (print { event: "celestial-event-created", event-id: event-id, creator: tx-sender })
        (ok event-id)
    )
)

(define-public (join-celestial-event
    (event-id uint)
    (accessibility-needs uint)
    (therapeutic-goals (string-ascii 256))
    (experience-level uint)
)
    (let ((event-data (unwrap! (map-get? celestial-events { event-id: event-id }) ERR_NOT_FOUND)))
        (asserts! (var-get contract-active) ERR_UNAUTHORIZED)
        (asserts! (get is-active event-data) ERR_NOT_FOUND)
        (asserts! (is-none (map-get? event-participants { event-id: event-id, participant: tx-sender })) ERR_ALREADY_EXISTS)
        (asserts! (< (get current-participants event-data) (get max-participants event-data)) ERR_EVENT_FULL)
        (asserts! (>= (get start-time event-data) stacks-block-height) ERR_SESSION_EXPIRED)

        ;; Check accessibility compatibility
        (asserts! (or (is-eq accessibility-needs u0)
                     (> (bit-and (get accessibility-features event-data) accessibility-needs) u0))
                 ERR_INVALID_INPUT)

        (map-set event-participants
            { event-id: event-id, participant: tx-sender }
            {
                joined-at: stacks-block-height,
                accessibility-needs: accessibility-needs,
                therapeutic-goals: therapeutic-goals,
                experience-level: experience-level,
                wonder-moments: u0,
                completion-status: "registered"
            }
        )

        (map-set celestial-events
            { event-id: event-id }
            (merge event-data { current-participants: (+ (get current-participants event-data) u1) })
        )

        ;; Initialize community member if new
        (if (is-none (map-get? community-members { member: tx-sender }))
            (map-set community-members
                { member: tx-sender }
                {
                    join-date: stacks-block-height,
                    total-sessions: u0,
                    total-wonder-moments: u0,
                    cosmic-perspective-level: u0,
                    accessibility-needs: accessibility-needs,
                    preferred-event-types: (pow u2 (- (get event-type event-data) u1)),
                    therapeutic-journey-notes: therapeutic-goals,
                    light-pollution-advocate: false,
                    mentor-status: false
                }
            )
            true
        )

        (print { event: "event-joined", event-id: event-id, participant: tx-sender })
        (ok true)
    )
)

;; ===================================================================
;; PUBLIC FUNCTIONS - TELESCOPE MANAGEMENT
;; ===================================================================

(define-public (register-telescope
    (model (string-ascii 128))
    (aperture-mm uint)
    (focal-length-mm uint)
    (location (string-ascii 256))
    (lat int)
    (lng int)
    (hourly-rate uint)
    (accessibility-features uint)
    (therapeutic-certified bool)
)
    (let ((telescope-id (var-get next-telescope-id)))
        (asserts! (var-get contract-active) ERR_UNAUTHORIZED)
        (asserts! (is-valid-coordinates lat lng) ERR_INVALID_INPUT)
        (asserts! (> aperture-mm u50) ERR_INVALID_INPUT) ;; Minimum 50mm aperture
        (asserts! (> focal-length-mm u200) ERR_INVALID_INPUT) ;; Minimum 200mm focal length

        (map-set telescopes
            { telescope-id: telescope-id }
            {
                owner: tx-sender,
                model: model,
                aperture-mm: aperture-mm,
                focal-length-mm: focal-length-mm,
                location: location,
                coordinates: { lat: lat, lng: lng },
                hourly-rate: hourly-rate,
                accessibility-features: accessibility-features,
                availability-start: stacks-block-height,
                availability-end: (+ stacks-block-height u525600), ;; 1 year default
                is-available: true,
                therapeutic-certified: therapeutic-certified,
                total-sessions: u0,
                rating-sum: u0,
                rating-count: u0
            }
        )

        (var-set next-telescope-id (+ telescope-id u1))
        (print { event: "telescope-registered", telescope-id: telescope-id, owner: tx-sender })
        (ok telescope-id)
    )
)

(define-public (reserve-telescope
    (telescope-id uint)
    (start-time uint)
    (duration-hours uint)
    (event-id (optional uint))
    (therapeutic-session bool)
)
    (let ((telescope-data (unwrap! (map-get? telescopes { telescope-id: telescope-id }) ERR_NOT_FOUND))
          (end-time (+ start-time (* duration-hours u144)))) ;; ~144 blocks per hour
        (asserts! (var-get contract-active) ERR_UNAUTHORIZED)
        (asserts! (get is-available telescope-data) ERR_TELESCOPE_UNAVAILABLE)
        (asserts! (and (> start-time stacks-block-height) (> duration-hours u0) (<= duration-hours u12)) ERR_INVALID_INPUT)
        (asserts! (is-none (map-get? telescope-reservations { telescope-id: telescope-id, start-time: start-time })) ERR_ALREADY_EXISTS)

        ;; Calculate payment
        (let ((payment-amount (* (get hourly-rate telescope-data) duration-hours)))
            ;; In a real implementation, handle STX payment here

            (map-set telescope-reservations
                { telescope-id: telescope-id, start-time: start-time }
                {
                    reserver: tx-sender,
                    end-time: end-time,
                    event-id: event-id,
                    payment-amount: payment-amount,
                    therapeutic-session: therapeutic-session,
                    created-at: stacks-block-height,
                    status: "confirmed"
                }
            )

            (print { event: "telescope-reserved", telescope-id: telescope-id, reserver: tx-sender, payment: payment-amount })
            (ok true)
        )
    )
)

;; ===================================================================
;; PUBLIC FUNCTIONS - THERAPEUTIC SESSIONS
;; ===================================================================

(define-public (create-therapeutic-session
    (event-id uint)
    (telescope-id (optional uint))
    (session-type (string-ascii 128))
    (duration-minutes uint)
    (therapeutic-goals (string-ascii 512))
    (facilitator principal)
)
    (let ((session-id (var-get next-session-id))
          (event-data (unwrap! (map-get? celestial-events { event-id: event-id }) ERR_NOT_FOUND)))
        (asserts! (var-get contract-active) ERR_UNAUTHORIZED)
        (asserts! (get is-active event-data) ERR_NOT_FOUND)
        (asserts! (is-some (map-get? event-participants { event-id: event-id, participant: tx-sender })) ERR_UNAUTHORIZED)
        (asserts! (and (> duration-minutes u0) (<= duration-minutes u480)) ERR_INVALID_INPUT) ;; Max 8 hours

        ;; Verify telescope availability if specified
        (match telescope-id
            tel-id (let ((telescope-data (unwrap! (map-get? telescopes { telescope-id: tel-id }) ERR_NOT_FOUND)))
                       (asserts! (get therapeutic-certified telescope-data) ERR_INVALID_INPUT)
                       true)
            true
        )

        (map-set therapeutic-sessions
            { session-id: session-id }
            {
                participant: tx-sender,
                event-id: event-id,
                telescope-id: telescope-id,
                session-type: session-type,
                duration-minutes: duration-minutes,
                therapeutic-goals: therapeutic-goals,
                facilitator: facilitator,
                start-time: (get start-time event-data),
                completion-status: "scheduled",
                wonder-score: u0,
                cosmic-perspective-gained: false,
                reflection-notes: "",
                created-at: stacks-block-height
            }
        )

        (var-set next-session-id (+ session-id u1))
        (var-set total-therapeutic-sessions (+ (var-get total-therapeutic-sessions) u1))

        (print { event: "therapeutic-session-created", session-id: session-id, participant: tx-sender })
        (ok session-id)
    )
)

(define-public (complete-therapeutic-session
    (session-id uint)
    (wonder-score uint)
    (cosmic-perspective-gained bool)
    (reflection-notes (string-ascii 1024))
)
    (let ((session-data (unwrap! (map-get? therapeutic-sessions { session-id: session-id }) ERR_NOT_FOUND)))
        (asserts! (var-get contract-active) ERR_UNAUTHORIZED)
        (asserts! (or (is-eq tx-sender (get participant session-data))
                     (is-eq tx-sender (get facilitator session-data))) ERR_UNAUTHORIZED)
        (asserts! (is-eq (get completion-status session-data) "scheduled") ERR_INVALID_INPUT)
        (asserts! (<= wonder-score u10) ERR_INVALID_INPUT) ;; 0-10 scale

        (map-set therapeutic-sessions
            { session-id: session-id }
            (merge session-data {
                completion-status: "completed",
                wonder-score: wonder-score,
                cosmic-perspective-gained: cosmic-perspective-gained,
                reflection-notes: reflection-notes
            })
        )

        ;; Update participant's wonder moments
        (update-wonder-metrics (get participant session-data) wonder-score)
        (var-set total-wonder-moments (+ (var-get total-wonder-moments) wonder-score))

        ;; Update event participant data
        (let ((event-participant (unwrap! (map-get? event-participants
                                         { event-id: (get event-id session-data),
                                           participant: (get participant session-data) }) ERR_NOT_FOUND)))
            (map-set event-participants
                { event-id: (get event-id session-data), participant: (get participant session-data) }
                (merge event-participant {
                    wonder-moments: (+ (get wonder-moments event-participant) wonder-score),
                    completion-status: "completed"
                })
            )
        )

        (print { event: "therapeutic-session-completed", session-id: session-id, wonder-score: wonder-score })
        (ok true)
    )
)

;; ===================================================================
;; PUBLIC FUNCTIONS - LIGHT POLLUTION ADVOCACY
;; ===================================================================

(define-public (submit-light-pollution-report
    (location (string-ascii 256))
    (lat int)
    (lng int)
    (pollution-level uint)
    (impact-description (string-ascii 512))
    (proposed-solutions (string-ascii 512))
)
    (let ((report-id (fold + (list u1 u2 u3) u0))) ;; Simple ID generation
        (asserts! (var-get contract-active) ERR_UNAUTHORIZED)
        (asserts! (is-valid-coordinates lat lng) ERR_INVALID_INPUT)
        (asserts! (and (>= pollution-level u1) (<= pollution-level u9)) ERR_INVALID_INPUT)

        (map-set light-pollution-reports
            { report-id: report-id }
            {
                reporter: tx-sender,
                location: location,
                coordinates: { lat: lat, lng: lng },
                pollution-level: pollution-level,
                impact-description: impact-description,
                proposed-solutions: proposed-solutions,
                report-date: stacks-block-height,
                community-support: u0,
                resolution-status: "submitted"
            }
        )

        ;; Mark reporter as light pollution advocate
        (let ((member-data (default-to
                            { join-date: stacks-block-height, total-sessions: u0, total-wonder-moments: u0,
                              cosmic-perspective-level: u0, accessibility-needs: u0, preferred-event-types: u0,
                              therapeutic-journey-notes: "", light-pollution-advocate: false, mentor-status: false }
                            (map-get? community-members { member: tx-sender }))))
            (map-set community-members
                { member: tx-sender }
                (merge member-data { light-pollution-advocate: true })
            )
        )

        (print { event: "light-pollution-report-submitted", report-id: report-id, reporter: tx-sender })
        (ok report-id)
    )
)

(define-public (support-light-pollution-report (report-id uint))
    (let ((report-data (unwrap! (map-get? light-pollution-reports { report-id: report-id }) ERR_NOT_FOUND)))
        (asserts! (var-get contract-active) ERR_UNAUTHORIZED)
        (asserts! (not (is-eq tx-sender (get reporter report-data))) ERR_INVALID_INPUT)

        (map-set light-pollution-reports
            { report-id: report-id }
            (merge report-data { community-support: (+ (get community-support report-data) u1) })
        )

        (print { event: "light-pollution-report-supported", report-id: report-id, supporter: tx-sender })
        (ok true)
    )
)

;; ===================================================================
;; READ-ONLY FUNCTIONS
;; ===================================================================

(define-read-only (get-celestial-event (event-id uint))
    (map-get? celestial-events { event-id: event-id })
)

(define-read-only (get-telescope (telescope-id uint))
    (map-get? telescopes { telescope-id: telescope-id })
)

(define-read-only (get-therapeutic-session (session-id uint))
    (map-get? therapeutic-sessions { session-id: session-id })
)

(define-read-only (get-community-member (member principal))
    (map-get? community-members { member: member })
)

(define-read-only (get-event-participant (event-id uint) (participant principal))
    (map-get? event-participants { event-id: event-id, participant: participant })
)

(define-read-only (get-telescope-reservation (telescope-id uint) (start-time uint))
    (map-get? telescope-reservations { telescope-id: telescope-id, start-time: start-time })
)

(define-read-only (get-light-pollution-report (report-id uint))
    (map-get? light-pollution-reports { report-id: report-id })
)

(define-read-only (get-contract-stats)
    {
        total-events: (- (var-get next-event-id) u1),
        total-telescopes: (- (var-get next-telescope-id) u1),
        total-therapeutic-sessions: (var-get total-therapeutic-sessions),
        total-wonder-moments: (var-get total-wonder-moments),
        contract-active: (var-get contract-active)
    }
)

(define-read-only (find-nearby-telescopes (lat int) (lng int) (max-distance uint))
    ;; In a production implementation, this would return a list of nearby telescopes
    ;; For now, return a simple structure indicating the search parameters
    {
        search-center: { lat: lat, lng: lng },
        max-distance: max-distance,
        search-time: stacks-block-height
    }
)

(define-read-only (get-upcoming-events (limit uint))
    ;; In a production implementation, this would return a list of upcoming events
    ;; For now, return metadata about the search
    {
        current-time: stacks-block-height,
        search-limit: limit,
        next-event-id: (var-get next-event-id)
    }
)

;; ===================================================================
;; ADMIN FUNCTIONS
;; ===================================================================

(define-public (set-contract-active (active bool))
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (var-set contract-active active)
        (print { event: "contract-status-changed", active: active })
        (ok true)
    )
)

(define-public (emergency-pause)
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (var-set contract-active false)
        (print { event: "emergency-pause-activated" })
        (ok true)
    )
)
