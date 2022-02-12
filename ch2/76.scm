;; When adding new types or generic operations via explicit dispatch
;; one must modify all the previously created operations and avoid
;; name conflicts.

;; Via data-directed programming we avoid both by installing packages
;; with all the operations tagged. This seems to be the most appropriate
;; when new types are often added.

;; Via message-passing style we also avoid both. This seems to be the most
;; appropriate when new operations are often added.
