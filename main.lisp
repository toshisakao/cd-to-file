(require :asdf)

; (ql:quickload '(:quri :dexador :jonathan))

(defparameter *user-agent* "LispMusicManager/0.0.1 ( muryuryumuryuryu@gmail.com )")
(defparameter *audio-file-exts* '("mp3" "flac" "m4a" "ogg"))

(defun get-dir-name (path)
  (car (last (pathname-directory path))))

(defun get-cache-path (artist album)
  (ensure-directories-exist ".cache/")
  (make-pathname :name (format nil "~a-~a" artist album)
                :type "json"
                :defaults ".cache/"))

(defun write-string-to-file (path string)
  (with-open-file (s path :direction :output :if-exists :supersede)
    (write-string string s)))

(defun read-file-to-string (path)
  (with-open-file (s path)
    (let ((data (make-string (file-length s))))
      (read-sequence data s)
      data)))

(defun get-album (artist album)
  (let ((cache-file (get-cache-path artist album)))
    (if (probe-file cache-file)
        (let ((data (read-file-to-string cache-file)))
          (jonathan:parse data))
        (get-album-api album artist))))

(defun get-album-api (album artist)
  (let* ((query (format nil "artist:~a AND release:~a" artist album))
         (url (format nil "https://musicbrainz.org/ws/2/release/?query=~a&fmt=json"
                      (quri:url-encode query)))
         (response (dex:get url :headers `(("User-Agent" . ,*user-agent*)))))
    (write-string-to-file (get-cache-path artist album) response)

    (sleep 1.1) ; be nice to the API
    (jonathan:parse response)))

(defun search-album-metadata (artist album)
    (let ((data (get-album artist album)))
      (let ((first-match (first (getf data :|releases|))))
        (if (not first-match)
            (format t "No matches found for \"~a\" - \"~a\"~%" artist album)
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
                  ; (format t "first-match: ~a~%~%" first-match)
                  (list :title  album-title
                        :artist artist-name
                        :date   album-date
                        :id     album-id))
                (format t "Warning: Exact match not found for \"~a\" - \"~a\" Found: \"~a\"~%"
                          artist album (getf first-match :|title|)))))))) ; TODO: make possible to choose from multiple results))
        

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
          
          (let* ((meta (search-album-metadata artist-name album-name)) ; TODO: fix so it refers to the file property instead of folder name
                 (mbid (getf meta :id)))
            
            (if (and mbid (not (probe-file (merge-pathnames "cover.jpg" album-dir))))
                (download-album-cover mbid album-dir)
                ; (format t "Going to download cover~%")
                (format t "Skipping ~a: Metadata not found or cover.jpg exists.~%" album-name))))))))

(defun put-cover-all (root-path)
  (dolist (artist-dir (uiop:subdirectories root-path))
    (dolist (album-dir (uiop:subdirectories artist-dir))
      (when (probe-file (merge-pathnames "cover.jpg" album-dir))
        (format t "Found cover in album: ~a~%" album-dir)
        (dolist (file-path (uiop:directory-files album-dir))
          ; (format t "Checking file: ~a and ~a~%"
          ;       (pathname-type file-path)
          ;       (member (string-downcase (pathname-type file-path)) *audio-file-exts* :test #'string-equal))
          
          (when (member (string-downcase (pathname-type file-path)) *audio-file-exts* :test #'string-equal)
            (put-on-cover (merge-pathnames "cover.jpg" album-dir) file-path)
          ))))))

(defun put-on-cover (cover-path file-path)
  (when (and (probe-file cover-path)
             (probe-file file-path))
    (format t "Putting cover on file: ~a~%" cover-path)
    (uiop:run-program (list "ffmpeg" "-y" "-i" (native-namestring file-path) "-i" (native-namestring cover-path)
      "-map" "0" "-map" "1" "-c" "copy"
      (format nil "~a.temp.~a" (native-namestring file-path) (pathname-type file-path)))) ; fix
    (uiop:delete-file-if-exists file-path)
    (uiop:rename-file-overwriting-target (format nil "~a.temp.~a" (namestring file-path) (pathname-type file-path)) file-path)
   ))

(defun read-config ()
  (with-open-file (stream "config")
    (let ((path (read-line stream nil nil)))
      path)))

(defun main ()
  (let ((root-path (read-config)))
    (format t "hello? ~a~%" root-path)
    (process-music-library root-path)
    (put-cover-all root-path)
    (format t "done~%")))
  

(main)