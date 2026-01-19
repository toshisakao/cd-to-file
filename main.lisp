(require :asdf)

; (ql:quickload '(:quri :dexador :jonathan))

(defparameter *user-agent* "LispMusicManager/0.0.1 ( muryuryumuryuryu@gmail.com )")

(defun get-dir-name (path)
  "Helper to get the last folder name as a string."
  (car (last (pathname-directory path))))

(defun search-album-metadata (artist album)
  (let* ((query (format nil "artist:~a AND release:~a" artist album))
         (url (format nil "https://musicbrainz.org/ws/2/release/?query=~a&fmt=json"
                      (quri:url-encode query)))
         (response (dex:get url :headers `(("User-Agent" . ,*user-agent*))))) ; Make the request with a required User-Agent
    ; (format t "query: ~a~%" query)
    
    (let ((data (jonathan:parse response)))
      (let ((first-match (first (getf data :|releases|))))
        (let* ((album-title (getf first-match :|title|))
               (album-date (getf first-match :|date|))
               (album-id (getf first-match :|id|))
               (artist-info (getf (first (getf first-match :|artist-credit|)) :|artist|))
               (artist-name (getf artist-info :|name|))
               (artist-sort-name (getf artist-info :|sort-name|)))
          
          (format  t "Artist name: ~a ~%" artist-name)
          (format  t "Artist sort name: ~a ~%" artist-sort-name)

          (if (or (equal album-title album) (equal artist-name artist))
            (progn
              (format t "Exact match found for \"~a\" - \"~a\"~%" artist album)
              (format t "first-match: ~a~%~%" first-match)
              (list :title  album-title
                    :artist artist-name
                    :date   album-date
                    :id     album-id))
            (format t "Warning: Exact match not found for \"~a\" - \"~a\" Found: \"~a\"~%"
                      artist album (getf first-match :|title|)))))))) ; TODO: make possible to choose from multiple results)
        

        

        

(defun download-album-cover (mbid destination-dir)
  (let ((url (format nil "https://coverartarchive.org/release/~a/front" mbid))
        (target-path (merge-pathnames "cover.jpg" destination-dir)))
    (handler-case
        (let ((image-data (dex:get url :want-stream nil)))
          (with-open-file (out target-path
                               :direction :output
                               :element-type '(unsigned-byte 8)
                               :if-exists :supersede)
            (write-sequence image-data out)
            (format t "Saved cover to: ~a~%" target-path)))
      (dex:http-request-failed (e)
        (format t "No cover found for ID ~a (Status: ~a)~%"
                mbid (dex:response-status e))))))

(defun process-music-library (root-path)
  (dolist (artist-dir (uiop:subdirectories root-path))
    (let ((artist-name (get-dir-name artist-dir)))
      (dolist (album-dir (uiop:subdirectories artist-dir))
        (let ((album-name (get-dir-name album-dir)))
          (format t "Processing: ~a - ~a~%" artist-name album-name)
          
          (let* ((meta (search-album-metadata artist-name album-name))
                 (mbid (getf meta :id)))
            
            (if mbid
                (download-album-cover mbid album-dir)
                (format t "Skipping ~a: Metadata not found.~%" album-name))
            
            ;; be slow to not get ip banned :)
            (sleep 1.2)))))))

(defun read-config ()
  (with-open-file (stream "config")
    (let ((path (read-line stream nil nil)))
      path)))

(defun main ()
    (process-music-library (read-config)))

(main)
; (test)