(require :asdf)

; (ql:quickload '(:quri :dexador :jonathan))

(defparameter *user-agent* "LispMusicManager/0.0.1 ( muryuryumuryuryu@gmail.com )")

(defun get-dir-name (path)
  "Helper to get the last folder name as a string."
  (car (last (pathname-directory path))))

(defun build-music-map (root-path)
  (let ((library (make-hash-table :test 'equal)))
    (dolist (artist-dir (uiop:subdirectories root-path))
      (let ((artist-name (get-dir-name artist-dir))
            (albums (make-hash-table :test 'equal)))
        (dolist (album-dir (uiop:subdirectories artist-dir))
          (let ((album-name (get-dir-name album-dir)))
            (setf (gethash album-name albums) 
                  (uiop:directory-files album-dir))))
        (setf (gethash artist-name library) albums)))
    library))

(defparameter *my-music* (build-music-map "/mnt/ntfs1/muryuryu/Documents/音楽/買った/"))


(defun search-album-metadata (artist album)
  (let* ((query (format nil "artist:~a AND release:~a" artist album))
         (url (format nil "https://musicbrainz.org/ws/2/release/?query=~a&fmt=json" 
                      (quri:url-encode query)))
         ;; 1. Make the request with a required User-Agent
         (response (dex:get url :headers '(("User-Agent" . *user-agent*)))))
    
    ;; 2. Parse the JSON string into a Lisp Property List (plist)
    (let ((data (jonathan:parse response)))
      ;; 3. Navigate the nested data
      (let ((first-match (first (getf data :|releases|))))
        (list :title (getf first-match :|title|)
              :date  (getf first-match :|date|)
              :id    (getf first-match :|id|))))))

(defun download-album-cover (mbid destination-dir)
  (let ((url (format nil "https://coverartarchive.org/release/~a/front" mbid))
        (target-path (merge-pathnames "cover.jpg" destination-dir)))
    (handler-case
        (let ((image-data (dex:get url :want-stream nil)))
          (with-open-file (out target-path
                               :direction :output
                               :element-type '(unsigned-byte 8) ;; Binary mode
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
          
          ;; 1. Get Metadata
          (let* ((meta (search-album-metadata artist-name album-name))
                 (mbid (getf meta :id)))
            
            ;; 2. If we found an ID, download the cover
            (if mbid
                (download-album-cover mbid album-dir)
                (format t "Skipping ~a: Metadata not found.~%" album-name))
            
            ;; 3. Respect MusicBrainz rate limit (1 req/sec)
            (sleep 1.2)))))))

(defun test ()
  (format t "Artists in my music library: ~a~%" 
          (alexandria:hash-table-keys *my-music*))
  ; get metadata for first album of first artist
  (let ((first-artist (first (alexandria:hash-table-keys *my-music*)))
        (first-album (first (alexandria:hash-table-keys 
                             (gethash (first (alexandria:hash-table-keys *my-music*)) 
                                      *my-music*)))))
    (let ((metadata (search-album-metadata first-artist first-album)))
      (format t "Metadata for ~a - ~a: ~a~%" first-artist first-album metadata))))

(defun main ()
  (process-music-library "/mnt/ntfs1/muryuryu/Documents/音楽/買った/"))

(main)