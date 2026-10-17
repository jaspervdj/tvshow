# tvshow

A dead-simple application to show picture albums residing on my hard drive on my
not-so-smart TV.

Usage:

    tvshow ~/Pictures/Album

An MJPEG stream will then be available at `http://localhost:8080/stream`, and an
HTML page with controls to go to the next or previous picture can be accessed at
`http://localhost:8080/`.  The controls page is designed to work on mobile.
