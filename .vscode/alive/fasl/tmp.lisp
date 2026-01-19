(require :asdf)

; (ql:quickload '(:quri :dexador :jonathan))

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
         (response (dex:get url :headers '(("User-Agent" . "MyLispMusicApp/1.0 ( mail@example.com )")))))
    
    ;; 2. Parse the JSON string into a Lisp Property List (plist)
    (let ((data (jonathan:parse response)))
      ;; 3. Navigate the nested data
      (let ((first-match (first (getf data :|releases|))))
        (list :title (getf first-match :|title|)
              :date  (getf first-match :|date|)
              :id    (getf first-match :|id|))))))

(defun main ()
  ;; Example usage of *my-music*
  (format t "Artists in my music library: ~a~%" 
          (alexandria:hash-table-keys *my-music*))
  ; get metadata for first album of first artist
  (let ((first-artist (first (alexandria:hash-table-keys *my-music*)))
        (first-album (first (alexandria:hash-table-keys 
                             (gethash (first (alexandria:hash-table-keys *my-music*)) 
                                      *my-music*)))))
    (let ((metadata (search-album-metadata first-artist first-album)))
      (format t "Metadata for ~a - ~a: ~a~%" first-artist first-album metadata))))

(main)