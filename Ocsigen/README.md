Pathname, Ocsigen Version
=========================

Ocsigen module to use pathnames as list or string.

### Available functions

See ```pathname.eliomi``` file.

### Example

      let path = Pathname.new_path_of_string "images/funny/dog.jpg" in
      Lwt.return
        (html
	   (head (title (pcdata "Ocsigen Pathname example")) [])
           (body [img
		    ~alt:(Pathname.no_extension path)
		    ~src:(make_uri
			    ~service:(Eliom_service.static_dir ())
			    (Pathname.to_list path)) ()]))

