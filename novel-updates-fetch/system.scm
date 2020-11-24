(module (novel-updates system javier) *
  (import (chicken base)
	  scheme
	  r7rs
	  (novel-updates main)
	  (novel-updates uri)
	  (srfi 1)
	  (srfi 13)
	  (srfi 18)
	  (srfi 28))

  ;;;;
  ;;;; The list of novels to fetch
  (define *seed-urls*
    '( ( "links_a-wild-last-boss-appeared" .
	 "https://www.novelupdates.com/series/a-wild-last-boss-appeared/")
       ( "links_blunt-type-ogre-girls-way-to-live-streaming" .
	 "https://www.novelupdates.com/series/blunt-type-ogre-girls-way-to-live-streaming/")
       ( "links_clearing-an-isekai-with-the-zero-believers-goddess-the-weakest-mage-among-the-classmates" .
	 "https://www.novelupdates.com/series/clearing-an-isekai-with-the-zero-believers-goddess-the-weakest-mage-among-the-classmates/" )
       ( "links_everyone-else-is-a-returnee" .
	 "https://www.novelupdates.com/series/everyone-else-is-a-returnee/" )
       ( "links_i-alone-level-up" .
	 "https://www.novelupdates.com/series/i-alone-level-up/" )
       ( "links_i-favor-the-villainess" .
	 "https://www.novelupdates.com/series/i-favor-the-villainess/" )
       ( "links_i-reincarnated-as-a-noble-girl-villainess-but-why-did-it-turn-out-this-way-ln" .
	 "https://www.novelupdates.com/series/i-reincarnated-as-a-noble-girl-villainess-but-why-did-it-turn-out-this-way-ln/")
       ( "links_i-reincarnated-as-a-noble-girl-villainess-but-why-did-it-turn-out-this-way" .
	 "https://www.novelupdates.com/series/i-reincarnated-as-a-noble-girl-villainess-but-why-did-it-turn-out-this-way/" )
       ( "links_itai-no-wa-iya-nanode-bogyo-ryoku-ni-kyokufuri-shitai-to-omoimasu" .
	 "https://www.novelupdates.com/series/itai-no-wa-iya-nanode-bogyo-ryoku-ni-kyokufuri-shitai-to-omoimasu/" )
       ( "links_ill-become-a-villainess-that-will-go-down-in-history" .
	 "https://www.novelupdates.com/series/ill-become-a-villainess-that-will-go-down-in-history/" )
       ( "links_kuma-kuma-kuma-bear" .
	 "https://www.novelupdates.com/series/kuma-kuma-kuma-bear/" )
       ( "links_life-with-a-tail" .
	 "https://www.novelupdates.com/series/life-with-a-tail/" )
       ( "links_only-sense-online" .
	 "https://www.novelupdates.com/series/only-sense-online/" )
       ( "links_saving-80000-gold-in-an-another-world-for-retirement" .
	 "https://www.novelupdates.com/series/saving-80000-gold-in-an-another-world-for-retirement/" )
       ( "links_tag-team" .
	 "https://www.novelupdates.com/series/tag-team/" )
       ( "links_the-villain-daughter-enjoys-her-seventh-life-as-a-free-spirited-bride-hostage-in-a-former-enemy-country" .
	 "https://www.novelupdates.com/series/the-villain-daughter-enjoys-her-seventh-life-as-a-free-spirited-bride-hostage-in-a-former-enemy-country/" )
       ( "links_the-villains-need-to-save-the-world" .
	 "https://www.novelupdates.com/series/the-villains-need-to-save-the-world/" )
       ( "links_when-i-reincarnated-i-was-one-of-the-villainess-followers-but-i-decided-to-live-my-life-freely" .
	 "https://www.novelupdates.com/series/when-i-reincarnated-i-was-one-of-the-villainess-followers-but-i-decided-to-live-my-life-freely/" )
       ( "links_while-killing-slimes-for-300-years-i-became-the-max-level-unknowingly" .
	 "https://www.novelupdates.com/series/while-killing-slimes-for-300-years-i-became-the-max-level-unknowingly/" )
       ))


  ;;;;
  ;;;; Fetch links for all given novels (as alist (dirname, seed-url)
  (define (fetch-all novels-alist)
    (for-each
     (lambda (dir-url)
       (newline)
       (display "---------------------------------")
       (newline)
       (display (format "Starting ~a" dir-url))
       (newline)
       (let* ((output-filename (car dir-url))
	      (seed-url (cdr dir-url))
	      (seconds 10)
	      (state (start-simple-system output-filename
					  seconds
					  seed-url))
	      (index-thread (first state))
	      (writer-thread (second state)))
	 (thread-join! index-thread)
	 (thread-join! writer-thread)))
     novels-alist))


  )
