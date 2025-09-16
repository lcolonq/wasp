;;; wasp-dna --- Activate the cloning device -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 's)
(require 'f)
(require 'ht)
(require 'wasp-ai)
(require 'wasp-fakechat)

;; swab DNA
;; (-non-nil
;;  (--map-indexed
;;   (when (s-equals? (car it) "Must_Broke_")
;;     (cons
;;      (nth (- it-index 1) fig//incoming-chat-history)
;;      it))
;;   fig//incoming-chat-history)))
;; (-non-nil (--map-indexed (when (s-equals? (car it) "pnutonium") (cons (nth (- it-index 1) test-messages-parsed) it)) test-messages-parsed))

(defun w/dna-get-all-log-paths ()
  "Return the paths of every log file."
  (--filter (s-matches? (rx "log-" (+ digit) "-" (+ digit) "-" (+ digit) ".txt") it) (f-files "~/logs")))

(defun w/dna-complete-log ()
  "Return a complete log of every historical message."
  (let* ((paths (w/dna-get-all-log-paths))
         (logstr (apply #'s-concat (-map #'w/slurp paths)))
         (log (--map (w/list-to-pair (s-split ": " it)) (-non-nil (--map (cadr (s-split "\t" it)) (s-lines logstr))))))
    log))

(defun w/dna-user-log (user)
  "Return a complete log of every message sent by USER."
  (cond
    ((s-equals? user "fake_test_user")
      (-repeat 100 (cons "fake_test_user" "hello i'm the test user haha i love apples and oranges")))
    (t
      (--filter
        (s-equals? (s-downcase (car it)) (s-downcase user))
        (w/dna-complete-log)))))

(defun w/dna-generate-from-logs (user)
  "Generate DNA from historical logs for USER.
You probably want to use this interactively and then save the result here."
  (let* ((log (w/dna-complete-log)))
    (-non-nil
     (--map-indexed
      (when (and (car it) (s-equals? (s-downcase (car it)) (s-downcase user)))
        (cons (nth (- it-index 1) log) it))
      log))))

(defun w/dna-user-clones (user)
  "Return the list of clones for USER."
  (--filter
   (s-equals?
    (s-downcase
     (w/fake-chatter-profile-username (w/fake-chatter-profile it)))
    (s-downcase user))
   w/fake-chatters))

(defconst w/dna-must_broke_
  '((("Hexadigital" . "Hi MODCLONK! hexadiHello") "Must_Broke_" .
     "modclonk is the only person i know that still playing the fruit game...")
    (("JDDoesDev" . "shebang shebang... oh baby") "Must_Broke_" .
     "enjoy your nap modclonk")
    (("FairchildVT" . "Must!") "Must_Broke_" . "Fair! lcolonHi")
    (("Lokiharth" . "I am extra buttoms for mouse enjoyer peepoHappy")
     "Must_Broke_" . "FeelsStrongMan")
    (("imgeiser" .
      "YOU SHOUL PLAY NIER AUTOMATA NOW! *thunder* (game is depression inducing)")
     "Must_Broke_" . "bless u")
    (("pwnedary" . "Joel") "Must_Broke_" . "Joel")
    (("GenDude" . "Heo mod! Joel") "Must_Broke_" . "lcolonHi modclonk")
    (("prodzpod" . "we are at least 15 thousand dollars indeed")
     "Must_Broke_" . "the 50K is already worth it, we're at NASA level")
    (("imgeiser" . "ask uwu for the furnace") "Must_Broke_" .
     "only 1K now wwparaSad")
    (("DoctorGlitchy" .
      "You can't make a perfect recreation unless you have nasa grade supercritical furnaces ;-;")
     "Must_Broke_" . "show us your clones doctor wwparaStare")
    (("saferq" . "@DoctorGlitchy so you admit they were clones")
     "Must_Broke_" . "beautiful clonkheads")
    (("NikolaRadHristov" . "wat") "Must_Broke_" .
     "we cant say here wwparaStare")
    (("GenDude" . "Germans? More like GERMans") "Must_Broke_" .
     "get momomo here")
    (("saferq" .
      "I mean how often do you sequence dna without blowing it apart and putting it back together")
     "Must_Broke_" . "unipiuScared")
    (("DoctorGlitchy" . "Not cloneing just science") "Must_Broke_" .
     "the socks baby...")
    (("dwinkley_" . "@DoctorGlitchy this is too science for me")
     "Must_Broke_" . "do i need to give you my spit? wwparaStare")
    (("Azrhyga" . "DNA is an helix, or the government lied to me?")
     "Must_Broke_" . "can you see it")
    (("divorce_jonZe" .
      "i put the hot scope on my left eye and the cold scope on my right")
     "Must_Broke_" . "$20,000 clonkhead vs $1 clonkhead")
    (("imgeiser" . "VAI BRASIL PORRA") "Must_Broke_" . "kino.")
    (("CrazyKitty357" .
      "just spend more points jetsLUL (this message was sent from ChatGPT)")
     "Must_Broke_" . "those 4K extra points...")
    (("GenDude" .
      "Its a fake clone to distract you from the doxxing. He is outside your house.")
     "Must_Broke_" . "CRAZY KITTY IS CHEATING")
    (("pablo_pepe_69" . "PERFECTION") "Must_Broke_" .
     "doubters cant get it")
    (("saferq" . "clone, clonq, connect the dots sheeple") "Must_Broke_"
     . "i will not lick anything ever again")
    (("DoctorGlitchy" . "OH GOD") "Must_Broke_" .
     "what type of avant-garde shit is this")
    (("LeadenGin" . "He's selling kanye clones for $1?? wow")
     "Must_Broke_" . "uwutooConcern")
    (("fabibiusus" .
      "Milliards is used for billions in a few European languages.")
     "Must_Broke_" . "24K is a scary number")
    (("Bezelea" . "was the lever count intentional?") "Must_Broke_" .
     "wwparaStare wwparaStare")
    (("prodzpod" . "oh i was talking about the wikipedia donation popup")
     "Must_Broke_" .
     "https://cdn.discordapp.com/attachments/779073687546232892/1184230427649642556/GBF2_iLWgAAQ9ID.png already posted on twitter but sending again cus the funny")
    (("fn_lumi" .
      "toast is too fancy??? i guess amazon not doing so great rn")
     "Must_Broke_" . "based mr green enjoyer")
    (("imgeiser" . "wait") "Must_Broke_" . "sex")
    (("steeledshield" . "📈") "Must_Broke_" .
     "i only need boosts in my wallet")
    (("pwnedary" . "Joel?") "Must_Broke_" . "Joel?")
    (("DoctorGlitchy" . "Joel") "Must_Broke_" . "Joel")
    (("CodeSpace0x25" . "so just dm me") "Must_Broke_" . "jesas")
    (("pablo_pepe_69" . "100k fuck me sideways") "Must_Broke_" .
     "EZ Clap")
    (("CodeSpace0x25" . "I’ll try to help with what I can @oakspirit")
     "Must_Broke_" . "imagine")
    (("ZenyaHima" . "very cool slideshow clonk") "Must_Broke_" .
     "uwutooConcern")
    (("ZenyaHima" . "INSANECAT") "Must_Broke_" . ":3")
    (("prodzpod" .
      "i played oub today, the map got bigger compared to the last time")
     "Must_Broke_" . "wwparaDance prog wwparaDance")
    (("pablo_pepe_69" . "[0.0]") "Must_Broke_" . "Christ.")
    (("DoctorGlitchy" . "Clueless") "Must_Broke_" . "ancient clonk stuff")
    (("pablo_pepe_69" . "streamer > mods > VIPs > soil > viewers")
     "Must_Broke_" . "us? wwparaSad")
    (("pablo_pepe_69" . "<3 <3 <3 MC") "Must_Broke_" .
     "i need to star calling her MC for now on")
    (("liquidcake1" . "WTF is going on here?") "Must_Broke_" .
     "classic clonkhead")
    (("DigbyCat" . "I was expecting number 1 to be modclonq")
     "Must_Broke_" . "forsenE")
    (("prodzpod" . "the integration award") "Must_Broke_" .
     "FORSEN MAH MAN")
    (("pablo_pepe_69" . "LUL LUL LUL LUL") "Must_Broke_" . "Clap")
    (("flyann" . "holy crap lois its huey hexadecimal") "Must_Broke_" .
     "unipiuScared")
    (("pablo_pepe_69" .
      "Kreygasm Kreygasm Kreygasm Kreygasm POWERPOINT STREAM")
     "Must_Broke_" . "wwparaPog numbers?")
    (("prodzpod" . "wrapup!!") "Must_Broke_" . "wwparaPog")
    (("imgeiser" . "I was in some people top, I am shocked")
     "Must_Broke_" . "lcolonCool numbers cool")
    (("Wina" . "archive.org is top 5 websites of all time tbh")
     "Must_Broke_" . "wwparaShake")
    (("eudemoniac" . "ogre build") "Must_Broke_" . "im an archive freak")
    (("DoctorGlitchy" . "Huh???") "Must_Broke_" . "🔥🔥🔥")
    (("eudemoniac" . "we should make our entry into the rap space, boss")
     "Must_Broke_" . "will we get a funny badge for this recap?")
    (("shindigs" . "CLONKING") "Must_Broke_" . "lcolonHi")))

(defconst w/dna-tyumici
  '((("Must_Broke_" . "the dopples are evolving Clap") "Tyumici" .
     "where's the stream readme")
    (("prodzpod" . "oh did tyumici clone redeemed") "Tyumici" .
     "dopple is good")
    (("carlossss333" . "live cloning") "Tyumici" . "I'm 90% cloneable")
    (("Tyumici" . "I'm 90% cloneable") "Tyumici" . "clone me up")
    (("prodzpod" . "im more of a shapez guy though") "Tyumici" .
     "aw hell yea, same")
    (("DigbyCat" . "Well \"not shit\" relative to windows") "Tyumici" .
     "no way 11 is not shit")
    (("ocuxw" . "is pacman have a issues?") "Tyumici" .
     "I love being able to say I use Arch (btw)")
    (("pwnedary" . "goodnight computer") "Tyumici" .
     "midi can do everything")
    (("ConditionBleen" . "lmao") "Tyumici" .
     "it's practically stolen valor")
    (("steeledshield" .
      "web dev often involves a lot of idiot proofing and telling it NOT to do certain stuff")
     "Tyumici" . "It pays the bills tho")
    (("Crane0001" .
      "I’m at work and can’t hear you but still wanted to say hi")
     "Tyumici" .
     "I wanted to do that as well, but yea work aoc balance was not feasible")
    (("Azrhyga" .
      "LCOLONQ is not any anime girl, is an ASCII anime girl inside of a computer with friend")
     "Tyumici" . "Oh no I meant for next year, the anarchy arc lol")
    (("steeledshield" . "yay!") "Tyumici" .
     "2024: Clonq does wrong things?")
    (("terriakijerky" . "this year has sucked for me jnero1LoamPensive")
     "Tyumici" . "this year was a lot")))

(defconst w/dna-pnutonium
  '((("baxtercrook" . "lcolonHi chubohHello") "pnutonium" .
     "this isnt singing this is yappin FR")
    (("lcolonq" .
      "votsirHdaRalokiN: -12, O87OP: -9, eiwets_yzarc: -7, YTT1KYZ4RC: -6, ynnuBnwotknaD: -6")
     "pnutonium" . "oh you did a good job though")
    (("liquidcake1" . "Well done, Friend!") "pnutonium" .
     "jol jol jol jol jol jol joljoljoljol")
    (("steeledshield" . "*yo") "pnutonium" .
     "i could probably manually pitch that to mariah carey vocals")
    (("liquidcake1" . "What's on the B-side?") "pnutonium" .
     "number of songs")
    (("steeledshield" .
      "I watched the batman animated series episode that has that version of jingle bells in it recently")
     "pnutonium" . "@liquidcake1 robin laid an egg")
    (("lcolonq" . "Not even a nibble...") "pnutonium" . "discord irc")
    (("lcolonq" . "https://discord.gg/f4JTbgN7St") "pnutonium" . "IRC")
    (("practicalnpc" . "are you excited for this upcoming year?")
     "pnutonium" . "cant promise oit")
    (("zom_danni" . "hope you are having a great day dude!") "pnutonium"
     . "happy virtual holidays frien (:")
    (("a_tension_span" .
      "I just got ads for cat food. It's good to know that twitch has so little data on me to still don't get me the right ads.")
     "pnutonium" . "we 80% in for the shits and gigs anyways")
    (("pwnedary" . "modclonk spotted?") "pnutonium" . "hi modclonk")
    (("gendude" . "I've seen where you lurk") "pnutonium" .
     "my pc just bluescreened D:")
    (("gendude" . "Happy U-Haul-iDays") "pnutonium" . "merry cridmuh")
    (("a_tension_span" .
      "vtube studio can do custom shaders only now? How is that not a day1 feature?")
     "pnutonium" . "its joever for colonq")
    (("h_ingles" . "Every vtuber knows you! A vtuber's vtuber.")
     "pnutonium" . "x list soon <3")
    (("harrisamapon" . "Stopping by from Sciants shout out to you")
     "pnutonium" .
     "I'll influence you to delete your OS files")))

(defconst w/dna-ellg
  '((("prodzpod" . "what the fuck") "ellg" .
     "every song is better when its 20% faster")
    (("Ricardo_Stryki" . "clonk clones") "ellg" .
     "remember the make the clone really into javascript (web) and nightcore")
    (("WUOTE" . "the only thing that brings me true joy is ACID") "ellg"
     . "ill take a ellg and a ellg (gay) clone")
    (("WUOTE" .
      "i love backseating tho! especially world record holders, becomes progressively harder over time rem2emWoozy")
     "ellg" . "you owe me like 2 clones at this point tbh")
    (("ellg" . "you owe me like 2 clones at this point tbh") "ellg" .
     "NOW: clone (real)")
    (("MODCLONK" . "lmao") "ellg" . "its ok to look hot clonk")
    (("prodzpod" . "he earthbends on gofd") "ellg" .
     "i dont think thats true")
    (("prodzpod" . "@yiffweed Factorio is such a good game") "ellg" .
     "do people ever not believe modclonk when she says shes married")
    (("Tomaterr" . "do you have a day of butlerian jihad") "ellg" .
     "lol i knew you were that kind of guy")
    (("MODCLONK" . ">:(") "ellg" .
     "whens the last time you got a haircut")
    (("ZedZark" . "you say \"abandoned\" but you still have them") "ellg"
     . "it was on the docket last week!")
    (("ellg" . "it was on the docket last week!") "ellg" .
     "speaking of add, dont forget to clone today >:)")
    (("ZedZark" .
      "maybe you can convince yourself that playing a game is productive work")
     "ellg" .
     "ya for sure, im just saying, its a good thing to at least try")
    (("babanana_7" . "relatable") "ellg" .
     "theres a lot of great resources on how to start that whole thing")
    (("stoicmana" . "imagine just having fun") "ellg" . "start there")
    (("prodzpod" .
      "i saw something like that happen in a lot of my friend who love mmos")
     "ellg" . "do you meditate at all clonk")
    (("KuromaruOniisan" . "by violating the intersate commerce clause")
     "ellg" .
     "modclonk seems the type that could get you some dank nugs (in minecraft)")
    (("khargoosh" . "The kids call it Doom Scrolling.") "ellg" . "lmao")
    (("khargoosh" . "I think the kids fall it Doom scrollibg.") "ellg" .
     "doing fat bong rips fixes a lot of things in life")
    (("Deep_field" . "err same*") "ellg" .
     "kinda like getting into meditation")
    (("ellg" . "kinda like getting into meditation") "ellg" .
     "\"why am i playing this game i could be coding something cool \" took me a while to get over that")
    (("prodzpod" . "nethack.") "ellg" . "i have the same issue")
    (("Tomaterr" . "idk how weed would interact with your Psyche") "ellg"
     .
     "not even joking, you just need to slightly turn down the \"why am i wasting time\" part of your brain")
    (("prodzpod" .
      "but if you want to write a story and weave it into the game making a game with solid gameplay (something you would like playing) that hints or gestures to the story (something you would like making) can be an answer")
     "ellg" . "then you can get into the gaming zone")
    (("zulleyy3" .
      "I am certain that Frag is a big reason why people play Mario")
     "ellg" .
     "i think you just need to get into drugs and also always having a slight buzz")
    (("Tyumici" . "god gtfo is hard") "ellg" . "he wanted to play the og")
    (("bobbehs" .
      "have you tried GTFO. thats the real title. its a hardcore survival horror shooter with great atmosphere")
     "ellg" . "you wont make it 2 hours in")
    (("prodzpod" .
      "so you can just make up wild shit about the game but there is definitely some kind of story the game eludes to")
     "ellg" .
     "didnt you say you wanted to do a modded ff7 playthrough lol")
    (("bobbehs" .
      "i find the longevity in my games the being the skill ceiling. games where you can constantly get better")
     "ellg" . "impressive")
    (("yiffweed" . "My roguelike is a roguelike") "ellg" .
     "its super impressibe")
    (("prodzpod" .
      "inscryption was a mid story game but then it became a twisted roguelike through the dlc")
     "ellg" . "ya")
    (("khargoosh" .
      "My first plunge into games was choose your own story book on a computer. After that I was in love. I want an awesome story and great engagement.")
     "ellg" .
     "https://rheavenstudio.itch.io/heaven-studio have you seen this clonk")
    (("Deep_field" . "Tunnet was the game. Its pretty cool") "ellg" .
     "very good cheese")
    (("ellg" . "very good cheese") "ellg" .
     "i have a giant block of tillamook black label cheddar in my fridge")
    (("Deep_field" .
      "its cool to see rust in games. I remember playing some kind of networking game that was written in rust")
     "ellg" . "@Tomaterr their smoked cheddar is so good")
    (("mcollardx" . "Aged steel cheddar. /me makes a note.") "ellg" .
     "tyumici is so right")
    (("Spaecplex" . "add fucked up physics so we can strafejump") "ellg"
     . "https://creamery.wsu.edu/cougar-cheese/ get on this")
    (("LeadenGin" . "moon jumping") "ellg" . "comes in a can")
    (("khargoosh" . "You on Linux?") "ellg" . "best cheese on earth")
    (("wetslugs" . "i like the low friction grass") "ellg" .
     "you need to get some cougar cheddar @MODCLONK")
    (("Deep_field" . "sexy dithering") "ellg" . "smh")
    (("Spaecplex" . "woh") "ellg" .
     "$15 stick of butter fans in this channel")
    (("Faeliore" . "I'm a big raylib enjoyer") "ellg" .
     "i think im kinda becoming a raylib guy over sdl")
    (("Tomaterr" . "Kerrygold Sponsor Me") "ellg" .
     "raylib also good to look at")
    (("prodzpod" . "holy fuck the kanban board") "ellg" .
     "its really nice with rust enums and pattern matching stuff")
    (("khargoosh" . "Egui is great for Native Apps. I use it.") "ellg" .
     "you might like Iced clonk, its based around elm / fp ui patterns")
    (("ZedZark" . "(probably not immediate mode)") "ellg" .
     "but imgui has a lot of really cool 3rd party stuff and looks a lot nicer too imo")
    (("CamuiCh" . "Cute stumpy walking stumps") "ellg" .
     "the c bindings are really simple you just have to do some annoying rust -> c string stuff")
    (("prodzpod" .
      "hello can you briefly flash the second season official new model for a second")
     "ellg" . "i think id probably still just use imgui even in rust")
    (("ZamielPayne" . "you look good with redhair") "ellg" .
     "immediate mode guis are cool for debugging but idk if id ever build something complex, you basically half implement retained ui to get something useful")
    (("Crane0001" . "well that resolves it") "ellg" .
     "i mean for something that simple ya itll be fine, its more like running that on phones and batterys and stuff kinda get weird")
    (("MODCLONK" . "I forget also you can't hear me lmao") "ellg" .
     "its updating the ui once every like 60 fps")
    (("MODCLONK" . "@Crane0001 lmao I guess I am too") "ellg" .
     "ya, and itll kill your battery too")
    (("khargoosh" . "Iced or egui") "ellg" . "theres a few")
    (("WUOTE" . "probably this https://jsoncanvas.org/") "ellg" .
     "like where you draw boxes and text on")
    (("Faeliore" . "its just a format for infinite canvas apps") "ellg" .
     "infinite canvas")
    (("JDDoesDev" . "is Jason Canvas related to John Twitch?") "ellg" .
     "i have not")
    (("ShyRyan" . "phoenix live view shyryaPhoenix") "ellg" .
     "im gonna become an enterprise golang developer and stop all ui dev now")
    (("ZamielPayne" . "google ads is in dart") "ellg" .
     "dart only exists becuase of flutter")))

(defconst w/dna-hexadigital
  '((("abipolarcarp123" .
      "We must accept the limitless capacity to forget, like math operations. We have been given gifts to recall what we forget and to have learned at all is something to cherish tdogSmile")
     "hexadigital" .
     "I have absolutely no idea why but for some reason I associate the number 29 with you, and today is the 29th hexadiBirdbrain")
    (("zulleyy3" .
      "or rather... why have sync / async code in the first place")
     "hexadigital" .
     "LCOLONQ! LCOLONQ! LCOLONQ! Today is a LCOLONQ! day hexadiCoding")
    (("setolyx" . "bogaHey catch y'all later!") "hexadigital" .
     "Thanks for the stream, L:Q hexadiCoding")
    (("tomaterr" . "they're just trying things out") "hexadigital" .
     "It's weird because they only rolled back the artistic portion, the rest of the stuff for IRL streamers is still allowed")
    (("fighting_annelids" . "nice, good luck clonk") "hexadigital" .
     "Biochar is interesting since it doesn't add any nutrition to soil, but it has a ton of surface space for microbes (a teaspoon of biochar dust has the surface area of a football field), so it improves the soil health and lasts hundreds of years")
    (("azrhyga" .
      "LCOLONQ having LCOLONQ stream in it hair, breaking the matrix")
     "hexadigital" .
     "Yeah - I've been dehydrating and grinding most of my compost supplies to take with me whenever I move, but bones and meat tends to be a bit too greasy for that to work well, so I'm turning it into biochar and processing it in small portions alongside the rest of the pre-compost")
    (("blazynights" . "ovo") "hexadigital" .
     "It's a bit different from your birds but I've been making biochar out of chicken bones all day today so far hexadiGardening")
    (("azrhyga" . "Wow @Hexadigital, that was fast :0") "hexadigital" .
     "The power of alerts hexadiCoding")
    (("hexadigital" . "birds!") "hexadigital" . "hexadiHeart")
    (("lcolonq" . "test") "hexadigital" . "birds!")
    (("liquidcake1" .
      "I'm note sure if I we have Prime, though. Is it like Supreme?")
     "hexadigital" .
     "I fear for the day that Amazon doesn't think I am a student and Prime costs more than a Twitch sub - last time I told it that I am forever learning, always discovering new things, so hopefully that's an infinite student discount")
    (("liquidcake1" . "I'm an Ingerlander, yes.") "hexadigital" .
     "prime time")
    (("danktownbunny" . "These clones better fight to the death")
     "hexadigital" . "Friend doesn't know the composition of water...")
    (("wadderdragen" . "The Raiders whomst just joined") "hexadigital" .
     "unhydrogenated water")
    (("wadderdragen" . "Fried Homer Simpson") "hexadigital" .
     "is it taco tuesday or terabyte tuesday? can you eat a terabyte of tacos?")
    (("pnutonium" .
      "while Im less busy Im gonna say that clonq is my 2 most watched with 170 hours <3")
     "hexadigital" . "Still looking for a morse code streamer")
    (("wadderdragen" . "Sauce that makes you say \"mmmm sauce mmm\"")
     "hexadigital" .
     "The only other non-verbal streamer I know of is one I don't watch because of that - no chat history, and no way to know when they're speaking")
    (("lokiharth" . "the strongest lurker") "hexadigital" .
     "Top selling point: you can walk away for 10 minutes and read the chat when you get back")
    (("hexadigital" .
      "I lurk more than I chat, so I would never complain about a lurker!")
     "hexadigital" . "hexadiLurk")
    (("inspectordiameter" . "I spent 47hrs watching @LCOLONQ in 2023.")
     "hexadigital" .
     "I lurk more than I chat, so I would never complain about a lurker!")
    (("wadderdragen" . "Sauce that makes you generate a windows 10 key")
     "hexadigital" . "You showed up in ninth hexadiHello")
    (("liquidcake1" .
      "Maybe it's just the ones with the brightest avatars.")
     "hexadigital" .
     "The first person is the top viewer, top chatter, and top redeemer, I think - they've never bought a sub or bits or anything though")
    (("michaelhunt1122" . "maybe highest volume on stream") "hexadigital"
     .
     "The fifth \"loudest fan\" on mine hasn't been in the stream for about half a year, I think")
    (("hexadigital" .
      "It showed top viewers last year, now it's top chatters")
     "hexadigital" . "Maybe most redeems?")
    (("prodzpod" . ":gasp:") "hexadigital" .
     "It showed top viewers last year, now it's top chatters")
    (("vesdeg" .
      "nixos is reproducible? how about you reproduce with some bitches")
     "hexadigital" .
     "I wish Twitch would show who your top viewers were, rather than just chatters")
    (("yellowberryhn" . "what fruit game") "hexadigital" . "hexadiShock")
    (("must_broke_" .
      "modclonk is the only person i know that still playing the fruit game...")
     "hexadigital" . "Hi MODCLONK! hexadiHello")
    (("steeledshield" . "mods are sleeping, post arch propaganda")
     "hexadigital" .
     "Hey LCOLONQ! Hope you're having a Telemetry-free Tuesday hexadiCoding")))

(defconst w/dna-steeledshield
  '((("lcolonq" . "I use nix btw") "steeledshield" .
     "I keep meaning to play it, it looks... interesting")
    (("chaosfoundry" .
      "Threads reminds me of those fake Social media apps from like Simulacra.")
     "steeledshield" . "the future is being shown content you don't want")
    (("pattywhacksknickknacks" . "OH SHIT IT'S REAL CLONK HEAD HOURS")
     "steeledshield" .
     "myspace is back... in pog form, btw: https://spacehey.com/")
    (("takeda_harunobu" . "RIP replicant c:") "steeledshield" .
     "@FushigiDango I forget which short story it was from, but I remember some piece of world building where housing is cheap if you agree to be blasted with ads while you sleep")
    (("ladyvignette" . "miawerJAM miawerJAM miawerJAM") "steeledshield" .
     "sugoiDansu sugoiDansu sugoiDansu")
    (("ladyvignette" . "curzecPet curzecPet curzecPet") "steeledshield" .
     "lcolonSpin Good Stream LCOLONQ lcolonSpin")
    (("yukievt" .
      "small was impressive, then people wanted to hold giant photo frames in front of their face, then it'll get small again~")
     "steeledshield" .
     "hullo. sorry I missed everything the new model looks cool")
    (("bezelea" . "red4") "steeledshield" . "ooooh sick!")
    (("bezelea" . "@handryc_ Iosevka comfy, link in the desc")
     "steeledshield" . "#ffa500")
    (("bezelea" . "we're all about big things here") "steeledshield" .
     "Shadow emacs money gang")
    (("fairchildvt" . "Bocchi Clonk") "steeledshield" .
     "does LCOLONQ also have abs?")
    (("yukievt" . "[peeing intensifies]") "steeledshield" . "Joel")
    (("bezelea" . "audible") "steeledshield" . "the classic")
    (("pnutonium" . "@shwasteddd k gn") "steeledshield" .
     "lcolonWiggly lcolonWiggly lcolonWiggly lcolonWiggly")
    (("duhuhu_" . "yukievRoll dumfoxRoll earendRoll peachi29Roll")
     "steeledshield" . "that's around 1am I think")
    (("piglilith" . "weeewooweeooo") "steeledshield" .
     "that's part of the reason I missed the \"fight\" earlier")
    (("flyann" . "LOL") "steeledshield" . "cruelty squad lookin ass")
    (("fairchildvt" . "\"Elcolonk lands a critical blow!\"")
     "steeledshield" . "woah")
    (("floflodie" .
      "i'm dumb and humble but i also have superiority complex and impostor symdrom so it's well balanced")
     "steeledshield" . "live REKT reaction")
    (("pnutonium" . "Cheezy jail lmao") "steeledshield" . "cheese gamer")
    (("bezelea" . "real gamer hours") "steeledshield" .
     "chill twitch alerts to programming and study to")
    (("operatorboi" . "Man emacs sucks") "steeledshield" .
     "looks like he has earrings now")
    (("lcolonq" . "developers") "steeledshield" . "we clonk on")
    (("jakecreatesstuff" . "DinoDance DinoDance DinoDance")
     "steeledshield" . "lcolonSpin lcolonSpin lcolonSpin")
    (("auts__" . "code monkeys unite") "steeledshield" . "#800080 :)")
    (("kawaii_chibi_koneko" . "uwu_to_owo is how btw") "steeledshield" .
     "La Colo-nc")
    (("must_broke_" . "FeelsBadMan") "steeledshield" .
     "that transitioned nicely")
    (("wadderdragen" .
      "Alright I am gonna be honest, 2:34am stream will stay open but eyes will close. Stay ZAMNazing ty")
     "steeledshield" . "dorbFlushed")
    (("praetorzero" . "stinky birb") "steeledshield" . "unimpressed")))

(defconst w/dna-crazykitty357
  '((("gendude" . "WATAMELON") "crazykitty357" .
     "jetsWave (this message was sent from ChatGPT)")
    (("watchmakering" . "got a haircut") "crazykitty357" .
     "jetsLetsgo (this message was sent from ChatGPT)")
    (("holy_cow_420" .
      "this is like changing your fav programming language")
     "crazykitty357" . "code vs code (this message was sent from ChatGPT)")
    (("xivandroid" . "@deep_field php isn't cool") "crazykitty357" .
     "@steppedupinstruw jetsWave hi bot (this message was sent from ChatGPT)")
    (("steeledshield" . "Numie") "crazykitty357" .
     "@StefiSot here is the library I used https://pub.colonq.computer/~prod/toy/dbkai/ (this message was sent from ChatGPT)")
    (("agpen" . "yes") "crazykitty357" .
     "!resolution stream more than 4 times this year (this message was sent from ChatGPT)")
    (("andygraviti" .
      "have you seen the jerma stream where he plays that gambling game with a mr. green in it")
     "crazykitty357" . "lcolonGreen (this message was sent from ChatGPT)")
    (("kuromaruoniisan" . "can we sell boost call options?")
     "crazykitty357" . "double boost?")
    (("malbidion" .
      "@Deep_field i wonder how hard would it be for necrophiles in a zombie apocalypse")
     "crazykitty357" . "https://www.youtube.com/watch?v=51GIxXFKbzk")
    (("kuromaruoniisan" .
      "kuroma16Wiggle kuroma16Wiggle kuroma16Wiggle kuroma16Wiggle")
     "crazykitty357" .
     "jetsPackjam jetsPackjam jetsPackjam jetsPackjam jetsPackjam (this message was sent from ChatGPT)")
    (("agpen" . "Joel yumma yumma") "crazykitty357" .
     "jetsWave (this message was sent from ChatGPT)")
    (("ivellon" . "It wasn't a remake, it was a sequel") "crazykitty357"
     . "sea sharp (this message was sent from ChatGPT)")
    (("imgeiser" . "HIIII MODCLONK") "crazykitty357" .
     "GET THAT BAG jetsLetsgo (this message was sent from ChatGPT)")
    (("adohtq" .
      "i dont think i have ever heard somebody pronounce the v like that")
     "crazykitty357" . "Joeler (this message was sent from ChatGPT)")
    (("yukievt" . "Phaswell") "crazykitty357" .
     "resident evil 5 campaign extra (this message was sent from ChatGPT)")
    (("destinywaits" . "whats your choice of modern hex editor")
     "crazykitty357" . "rush e")
    (("destinywaits" . "the worst song ever") "crazykitty357" .
     "new notes")
    (("yukievt" . "very nice!") "crazykitty357" . "geiser")
    (("prodzpod" . "he forget.....") "crazykitty357" . "emacs lisp")
    (("destinywaits" . "modclonk's laugh") "crazykitty357" . "John emacs")
    (("prodzpod" .
      "oh yeah friend i did make a bloom filter for my game yesterday, finally cracked out some glsl")
     "crazykitty357" . "https://www.twitch.tv/rezuul jong")))

(defconst w/dna-jakecreatesstuff
  '((("king_george_3d" . "I want to see AI taking over engineering world")
     "jakecreatesstuff" . "lcolonCool")
    (("jonkero" . "vim*") "jakecreatesstuff" .
     "Can you make a command that works like a magic 8 ball, where you ask it a question and it gives clonk based answers?")
    (("jonkero" . "need a todo list to build the todo list")
     "jakecreatesstuff" . "hell yeah")
    (("girl_w_the_dragon_tattoo" . "@mickynoon, NODDERS")
     "jakecreatesstuff" . "@CodeSpace0x25 blue sweatpants")
    (("yellowberryhn" . "based on emacs") "jakecreatesstuff" . "woooo")
    (("prodzpod" . "oh someone redeemed chat command?")
     "jakecreatesstuff" . "I've been savin' got a while")
    (("setolyx" . "god i love stupid internet things LUL")
     "jakecreatesstuff" .
     "oh my god I hate that I like the name jakeball")
    (("steeledshield" . "HypeCute") "jakecreatesstuff" . "ayo friendo")
    (("azrhyga" . "Today is time to go the C underwater")
     "jakecreatesstuff" . "You're so brave")
    (("destinywaits" . "LMAO") "jakecreatesstuff" .
     "Oh we're working on the 8Ball?")
    (("jakecreatesstuff" . "Oh we're working on the 8Ball?")
     "jakecreatesstuff" . "Hell yeah")
    (("vvizualizer" .
      "I was washing my hair bbg, I'm back. How are things going?")
     "jakecreatesstuff" .
     "I am late but I am here. Just took the best nap of my life")
    (("lcolonq" . "how are you feeling today now that you can talk?")
     "jakecreatesstuff" . "It kinda sounds like a Don't Starve character")
    (("ryanwinchester_" . "LUL") "jakecreatesstuff" .
     "Fugi? Love that guy")
    (("ellg" .
      "are we gonna go over all the hype cppcon videos on stream today")
     "jakecreatesstuff" . "so many welcomes lcolonNodders")
    (("eudemoniac" . "godot-blessed Joel LETSGO") "jakecreatesstuff" .
     "Joel2")
    (("terriakijerky" . "a dog did a speedrun earlier")
     "jakecreatesstuff" . "my blue is not the same as your blue")
    (("tyumici" . "it was sick") "jakecreatesstuff" . "jakecr3CookieHi")
    (("tomaterr" . "new bits") "jakecreatesstuff" . "jakecr3CookieHi")))

(defconst w/dna-zulleyy3
  '((("jordanhothead" . "get it gedit") "zulleyy3" .
     "Oh is it actually gedit not g-edit?")
    (("loufbread_" . "new guy") "zulleyy3" .
     "The mortal yoke of breathing")
    (("spooksyvt" . "but that requires you to go outside") "zulleyy3" .
     "a different timeline where lcolonq programs animals")
    (("zulleyy3" . "a different timeline where lcolonq programs animals")
     "zulleyy3" . "Animals are basically flesh robots")
    (("ryanwinchester_" . "touhced grass. saw coyote. never again.")
     "zulleyy3" . "we can condition them after all")
    (("ryanwinchester_" .
      "it's not sitting down: it's not moving. standing is bad too if you don't move")
     "zulleyy3" .
     "It's actually really scary how well you can train animals. I sometimes get recommended shorts on \"how to make your cat be very cuddly\" and it's just \"get them use to your touch from early childhood\"")
    (("kyza_2k" . "you'd never have to get out of bed") "zulleyy3" .
     "if you change your posture all the time it's fine")
    (("jaafartrull" .
      "A spoke to someone who had the office next to RMS and he had severe carpal. He would tell interns what to type and yell at them when they got it wrong")
     "zulleyy3" .
     "When i have handpain, it's usually from my shoulder and just pinch my ulnar nerve (up to my hand)")
    (("kyza_2k" . "the commander") "zulleyy3" .
     "@InspectorDiameter yeah!")
    (("remlessthanthree" . "whatcha makin") "zulleyy3" .
     "ulnar pain is so uncomf")
    (("lcolonq" . "green") "zulleyy3" . "Monster Energy Clonq")
    (("abipolarcarp123" . "GoMoCo") "zulleyy3" . "More?")
    (("zulleyy3" . "More?") "zulleyy3" . "walfasWow")
    (("zulleyy3" . "walfasWow") "zulleyy3" . "Super Idol #Real")
    (("djkawaiifieri" . "HAPPY NEW YEAR!") "zulleyy3" .
     "i heard the tax things is him eating your food")
    (("kyza_2k" . "dude thats so cool. I'll never get over it")
     "zulleyy3" . "biboo is such a trooper")
    (("dwinkley_" . "Im more of a tax non-enjoyer") "zulleyy3" .
     "i am still trying to figure out if people just like making AI - covers with her or if it's her actually singing all these shitposts")
    (("lcolonq" . "https://twitter.com/LCOLONQ") "zulleyy3" . "LUL")
    (("djkawaiifieri" . "!resolution have rooftiLove") "zulleyy3" .
     "@MODCLONK bless holofans always linking a timestamp of the original in the description soyapoG")
    (("zulleyy3" .
      "@MODCLONK bless holofans always linking a timestamp of the original in the description soyapoG")
     "zulleyy3" . "it's real")
    (("spooksyvt" . "HAHAHAHHAHA") "zulleyy3" . "lol")
    (("kyza_2k" . "!resolution don'") "zulleyy3" .
     "yeah, the devil trigger made me thing \"there is no way people aren't using ai voice for this... this is unreal\"")))

(defconst w/dna-tomaterr
  '((("ellg" . "have you seen the unglued verison of this clonk")
     "Tomaterr" .
     "modtoma yesterday: \"this is like the live oyster shucking pearl MLM\"")
    (("zulleyy3" .
      "wait,,, black lotus is actually a good card isn't it...")
     "Tomaterr" .
     "he got scammed and then the backup pack was doo doo buttcheeks")
    (("eudemoniac" . "ICANT") "Tomaterr" . "madonn")
    (("stoicmana" . "kek") "Tomaterr" . "mama mia")
    (("a_tension_span" .
      "PoroSad PoroSad PoroSad PoroSad PoroSad PoroSad PoroSad PoroSad PoroSad PoroSad PoroSad PoroSad halfway through the year that just started last week.")
     "Tomaterr" . "45")
    (("prodzpod" .
      "why is my resolution something about \"arch life\" or whatever the fuck friend keeps talking about")
     "Tomaterr" . "people never scam in MtG")
    (("prodzpod" . "speaking of") "Tomaterr" .
     "all the starters still on the market are legit")
    (("HazmatVT" . "ITS NEXT WEEKEND") "Tomaterr" . "enjoy nap maude")
    (("HazmatVT" . "Man I'm so hyped for next weekend") "Tomaterr" .
     "I told her you and clonk met while he was an officer in the college's MtG club and she said \"damn maude was in the trenches\"")
    (("zulleyy3" . "no way") "Tomaterr" .
     "ellg I was watching that and modtoma said \"you know before you I didn't think men had rich inner lives but now I know that it's just stuff that they would get made fun of for by women if they talked about it\"")
    (("ellg" .
      "can we talk about that insane fake alpha starter from last night")
     "Tomaterr" . "I feel like living wall")
    (("khlorghaal" . "that one was good") "Tomaterr" .
     "most nights modtoma is always trying to fall asleep and I can't stop showing her psychic damage inducing tweets")
    (("prodzpod" . "haemato critical ??? is this a new mmo stat")
     "Tomaterr" . "how long did the drooling asesprite drawing take")
    (("liquidcake1" . "Blood doping.") "Tomaterr" .
     "she's a student of the auts (autistics)")
    (("khlorghaal" . "alchemist grindset") "Tomaterr" .
     "\"maude do you agree that I have a sick and twisted sense of humour??\"")
    (("Faeliore" . "blood transfusions as an oub mechanic?? lmao")
     "Tomaterr" . "bring the lcolonq telepresence bot")
    (("mickynoon" . "I’m back") "Tomaterr" .
     "you should bring a laptop and have clonk teleconference in")
    (("nichePenguin" . "launched any cdda recently clonk??") "Tomaterr" .
     "omg!!!!")
    (("HellPie" . "@Faeliore 3, 4, 5 and 6 all at once") "Tomaterr" .
     "oh nice!")
    (("machinemob" . "you would contract sepsis by the end of the day")
     "Tomaterr" . "no maude??")
    (("Faeliore" . "covid 2 ground zero") "Tomaterr" .
     "WHERE WILL YOU BE")
    (("khlorghaal" . "@a_tension_span smile emoji") "Tomaterr" .
     "I gotta talk more about modtoma too")
    (("babanana_7" . "is a mustache a beard") "Tomaterr" .
     "you'll be an egghead")
    (("HazmatVT" . "hazmat23Screm") "Tomaterr" .
     "tbh if you shave your beard your head will actually be egg mode")
    (("prodzpod" . "i think its like hair") "Tomaterr" .
     "it was in the80s? did they have square bottom ties? was there cocaine?")
    (("kierem__" . "Joel") "Tomaterr" . "how was store?")
    (("prodzpod" . "have good nap modclonk") "Tomaterr" .
     "I love omelettes")
    (("prodzpod" .
      "yeah the leaderboard goes friend, modclonk, ellg and eggs i think")
     "Tomaterr" . "I made eggs today")
    (("Tomaterr" . "I made eggs today") "Tomaterr" . "friend is number 1")
    (("prodzpod" . "[🌙] JoelCheck RECIEVE") "Tomaterr" . "JoelCheck")
    (("Tomaterr" . "JoelCheck") "Tomaterr" .
     "I do legit think you're cool as shit though modcolonq")
    (("prodzpod" . "oh yeah maude i planted a bot in this broadcast now")
     "Tomaterr" . "you weren't even a twink then")
    (("prodzpod" . "this is CINEMA") "Tomaterr" .
     "who remembers the microwave channel")
    (("prodzpod" . "this is KINO") "Tomaterr" . "this is so good")
    (("JonponMusic" . "LMAO") "Tomaterr" . "I'm seeding my bot rn maude")
    (("Tomaterr" . "I'm seeding my bot rn maude") "Tomaterr" .
     "she's very cool and good at games")
    (("JonponMusic" . "yo that's nice") "Tomaterr" .
     "I'm so glad modclonk is here now")
    (("MODCLONK" . "@khlorghaal +2 󠀀") "Tomaterr" .
     "thinking? that's dangerous")
    (("jazzahol_VT" . "hey it’s the knife from Dead Money") "Tomaterr" .
     "even in knives this dude can't stop making ikarugas")
    (("fn_lumi" . "i’ve never heard of this knife thing") "Tomaterr" .
     "my life is like a video game")
    (("StefiSot" .
      "someone do the thing with the bells, my life is no longer melodious")
     "Tomaterr" . "clonk yearns for a jimono")
    (("JonponMusic" . "How To Basic ?") "Tomaterr" .
     "I love weed and I love women and I want to live on a ranch with them and a gigabit internet connection")
    (("MODCLONK" . "@Tomaterr +2") "Tomaterr" .
     "gotta seed my chat with the good kush")
    (("ellg" . "https://i.ell.dev/LFTobaFL.jpg check it out clonk")
     "Tomaterr" . "they most definitely do not!")
    (("yellowberryHN" . "it's an apple of the pine") "Tomaterr" .
     "sexcolonq at it again")
    (("MODCLONK" . "(pineapple upside down cake is so good)") "Tomaterr"
     .
     "if you want to see a really funny movie about swinging then you should watch The Ice Storm")
    (("ryasuar" . "I've lost track of the conversation entirely")
     "Tomaterr" .
     "this sexual ass streamer missed my grand theory of esports because he can't stop talking about swinging")
    (("MODCLONK" . "@prodzpod I am SO JEALOUS I missed this!!!!")
     "Tomaterr" .
     "all shooters can be thought of as a form of competitive cinematography")
    (("yellowberryHN" .
      "@a_tension_span definitely, it's been a while. i used to host a insta server")
     "Tomaterr" . "ok so you want my grand theory of fps")
    (("prodzpod" .
      "finally we can breathe again now that the air is not full of male")
     "Tomaterr" . "hold on I return 2 secs")
    (("a_tension_span" . "@vvizualizer Which one, UT or Quake?")
     "Tomaterr" . "hi maude!!!")
    (("yellowberryHN" . "I went from quake to UT") "Tomaterr" .
     "MAUDE!!!!!")
    (("kierem__" . "Joel") "Tomaterr" . "because that's me af")
    (("Tomaterr" . "because that's me af") "Tomaterr" .
     "this is why I want to look at Big Women Big Good")
    (("JonponMusic" .
      "@a_tension_span straight on my playlist though thank you")
     "Tomaterr" .
     "because maude isn't here and 50% of my chat messages are me yelling at modclonk")
    (("a_tension_span" . "@JonponMusic lcolonCool") "Tomaterr" .
     "I mean today is a bad sample though")
    (("a_tension_span" .
      "@a_tension_span Gosh darnit yewtube, here: https://youtube.com/watch?v=SSR5u2xvW8M")
     "Tomaterr" . "lmfao")
    (("a_tension_span" .
      "@JonponMusic Thank me later: https://yewtu.be/watch?v=SSR5u2xvW8M")
     "Tomaterr" . "no, twomaterr is an accessory prodzpod")
    (("yellowberryHN" . "i really ought to play more of the UT games")
     "Tomaterr" . "she mon on my ad till I")
    (("prodzpod" .
      "zenzic feels too much like gen z, i think i like 8th power better for zzz purposes")
     "Tomaterr" . "Monad Collection")
    (("a_tension_span" . "@Tomaterr LUL NotLikeThis PoroSad") "Tomaterr"
     . "das vtuber")
    (("JonponMusic" . "@a_tension_span love it") "Tomaterr" .
     "germans love vtubing")
    (("prodzpod" .
      "@a_tension_span i thought it was like a chinese chess thing")
     "Tomaterr" . "who wants to play numberwang")
    (("ZedZark" . "according to the duckduckgo spell checker") "Tomaterr"
     . "there's also numberwang")
    (("eudemoniac" . "Zone Zone") "Tomaterr" . "WebZone")
    (("a_tension_span" . "I'm a tension") "Tomaterr" . "I'm tomaterr")
    (("nichePenguin" . "zone limesComfy") "Tomaterr" .
     "I always try to match the chat's freak")
    (("Tomaterr" . "I always try to match the chat's freak") "Tomaterr" .
     "same vvizualizer")
    (("eudemoniac" . "what's your take on bananas") "Tomaterr" .
     "Focusing on positivism in this time of dumb bullshit is revolutionary")
    (("ellg" . "@prodzpod oh my bad lol") "Tomaterr" .
     "prod it would be such an incredible reveal if the reason your js is the way it is was because of carbon monoxide")
    (("isomorphica" . "Also, SET and SETF") "Tomaterr" .
     "yeah your gender has always struck me as \"computer\"")
    (("prodzpod" .
      "vtubers were this and now the vtubers are innovating to stay grasping of this face")
     "Tomaterr" . "lmfao")
    (("Tomaterr" . "lmfao") "Tomaterr" . "gender? who needs it")
    (("fcollector" . "Parameter clobbers on load") "Tomaterr" .
     "bro you bait all these women into thinking you're a woman streamer and then we show up and once the eggbaiting wears off we go \"well at least there's modclonk and a bunch of other autistic women here so I guess I'll stay\"")))

(defconst w/dna-modclonk
  '((("azrhyga" . "Every day the starting screen is better and better")
     "modclonk" . "hi clonkheads!!!")
    (("azrhyga" . "@MODCLONK Hi MODCLONK!!") "modclonk" . "lcolonHi")
    (("jaruonic" . "omg is this from Paprika") "modclonk" .
     "@azrhyga HI!!!")
    (("xivandroid" .
      "https://en.m.wikipedia.org/wiki/Chaitin%27s_constant")
     "modclonk" . "thank you thank you")
    (("spooksyvt" . "keep thinking ur saying foreskin") "modclonk" .
     "notepad++ is S tier")
    (("dwinkley_" .
      "Im still young enough to be able to prevent having wrist pain thankfully")
     "modclonk" . "@SpooksyVT I'll get the hatchet")
    (("codespace0x25" . "hi @modclonk") "modclonk" .
     "@CodeSpace0x25 Hi!! lcolonHi")
    (("crazykitty357" .
      "c/Bc/AA/ABAGE//cedBG//BdcBcBA/c/Bc/AA/ABAGE//B/ed/BBd/cBcBA//cBca//cBcg//c/cg//cBcb/a//cBca//cBcg//c/cg//cBcb/a")
     "modclonk" . "https://youtu.be/GMMLgHC8wVE?si=M76290q8bybDarNI")
    (("zulleyy3" . "lol") "modclonk" . "hell yeah")
    (("must_broke_" . "biboo is such a memelord") "modclonk" .
     "https://youtu.be/4jiPlbrvbxM?si=zMFu3OGQZCttNJru")
    (("steeledshield" . "you're lying") "modclonk" .
     "!resolution make a game with clonk")
    (("spooksyvt" . "these lyrics are about me") "modclonk" .
     "He's totally serious")
    (("liquidcake1" .
      "Having other people listen to my music always makes me cringe.")
     "modclonk" . "it's been a thing for years")
    (("doctorglitchy" . "Should Colonq listen to my track?") "modclonk" .
     "so many long car rides with zero music")
    (("spooksyvt" .
      "whenever you arent protected by atleast 6 layers of irony ur basically naked")
     "modclonk" . "@morgvn_ literally our life")
    (("cecihimevt" . "@djkawaiifieri cecihiSmush") "modclonk" .
     "@OlgaOkami Hi! Happy New Year!! :D")
    (("injulyyy" .
      "man I can't find these themes for vim. They looks pretty")
     "modclonk" . "lcolonGreen")
    (("keitaroch" . "lcolonGGG") "modclonk" . "lcolonGGG")
    (("gendude" . "Joel") "modclonk" . "Hi chat!!")
    (("modclonk" . "Hi chat!!") "modclonk" . "Joel")
    (("darthchaos15" . "Hello. How is everyone?") "modclonk" . "Joeler")
    (("gendude" . "CLAB-o-rotatory") "modclonk" . "@GenDude Hi!!")
    (("must_broke_" . "goo evening modclonk lcolonGreen") "modclonk" .
     "@Azrhyga Hi!! lcolonHi")
    (("steeledshield" . "hello modclonk lcolonHappy") "modclonk" .
     "@Must_Broke_ Hello!! lcolonHi")
    (("modclonk" . "@Must_Broke_ Hello!! lcolonHi") "modclonk" .
     "@steeledshield Hi hi! Joel")
    (("eudemoniac" . "hello to all the C creatures Joel") "modclonk" .
     "@exodrifter_ HI EXO!!!")
    (("steeledshield" . "is it a what?") "modclonk" . "I love Kobo....")
    (("mickynoon" . "lcolonGreen") "modclonk" .
     "@steeledshield https://youtu.be/OMjYXVJDAy8?si=nCcDxt_3GJubou9y")
    (("azrhyga" . "An 8ball command will be great to have here")
     "modclonk" .
     "@GenDude The original stream or is there something Kobo is referencing?")
    (("gendude" . "@MODCLONK https://www.youtube.com/watch?v=Bu8bH2P37kY")
     "modclonk" . "Sadge")
    (("iantrudel" . "Jonkero sent me here.") "modclonk" .
     "@GenDude LMAO WAIT THANK YOU")
    (("ivellon" . "Also who is there in the background?") "modclonk" .
     "Hi!")
    (("modclonk" . "Hi!") "modclonk" . "I'm here!!!")
    (("gendude" . "https://www.youtube.com/watch?v=Bu8bH2P37kY")
     "modclonk" . "@Ivellon  lcolonHi lcolonHi")
    (("andygraviti" .
      "it can have ham but the more important parts are the greens and the black eyed peas")
     "modclonk" . "@liquidcake1 yes yes yes")
    (("zulleyy3" . "lcolonGreen") "modclonk" . "Noooooo")
    (("iloidtupo" . "hello joshua lcolonGreen") "modclonk" .
     "lcolonSadge lcolonSadge lcolonSadge")
    (("darpsyx" . "Michael Scott") "modclonk" . "@GenDude yes")
    (("sandbox_actual" .
      "Maybe some Japanese law thing?  Nintendo be like that too.")
     "modclonk" . "GAMING GAMING GAMING")
    (("zulleyy3" . "ffmpreg") "modclonk" . "lmao let's goooooo")
    (("azrhyga" .
      "Proton is best for run games, Wine have more issues, also you will have to use surely DXVK")
     "modclonk" . "@CodeSpace0x25 nope lcolonGreen")
    (("ivellon" . ":D") "modclonk" . "@liquidcake1 I feel this")
    (("qeswic" . "That looks awesome") "modclonk" .
     "@liquidcake1 this is actually so interesting")
    (("ginquoxx" . "Yeah") "modclonk" .
     "@liquidcake1 Y2K stuff is interesting to me")
    (("codespace0x25" . "boop") "modclonk" . "@FoggyRoses YEAH")
    (("vettle" .
      "If you ever want to do more hit me up I'm down to help out folks")
     "modclonk" . "@Vettle I'm jealous this is so sick")
    (("vettle" . "But as I have a good group its soured me on PF")
     "modclonk" . "@Vettle  lcolonLove lcolonLove lcolonLove")
    (("liquidcake1" .
      "I think euclidean space starts to be an issue when you crowd people in together. You need your world to be slightly hyperbolic/negatively curved.")
     "modclonk" . "@DestinyWaits one of us one of us")
    (("hexadigital" . "Hi MODCLONK!") "modclonk" . "@Hexadigital Hi!!")
    (("azrhyga" . "Thanks for the raid @Geop!! Also welcome raiders!!")
     "modclonk" . "Hi raiders!!!!! lcolonHi lcolonHi")))

(defconst w/dna-yellowberryhn
  '((("j_art_" .
      "In my country, \"family\" in the company is a complete sign to get out of there, as well as using English expressions unnecessarily (like an English word in the middle of a sentence that has nothing to do with it).")
     "yellowberryhn" . "we clinking and clonking")
    (("tyumici" . "!resolution 5120x1440") "yellowberryhn" .
     "i saw that and i literally thought \"oh i bet he's collecting people's screen resolutions in celebration of new years\"")
    (("stuxvt" . "types help compiler be better too") "yellowberryhn" .
     "why are we talking about kanban boards?")
    (("stuxvt" . "faster compiled code") "yellowberryhn" .
     "i think i missed it")
    (("tyumici" . "Gonna give Atlassian $3000 a year for Jira?")
     "yellowberryhn" . "sounds like a good time")
    (("saferq" . "hit us with the OUB") "yellowberryhn" .
     "!resolution stream once a week")
    (("zulleyy3" .
      "Then I noticed how it takes me ages to make updates to a tiny docker file (not my fault i had to find out which pip wheels on pypi allow you to actually install cuda) and all then manually test. Got fed up enough and now I will just write tests so i don't go insane over the tiniest thing walfasLaugh")
     "yellowberryhn" .
     "i think i'm gonna start doing streams on wednesday")
    (("a_tension_span" . "yanderaeCheer yanderaeCheer freamRave")
     "yellowberryhn" . "yellow166Jam yellow166Jam yellow166Jam")
    (("dj_zmx" .
      "@zulleyy3 building external stuff is definitely the worst thing about packaging nowadays")
     "yellowberryhn" . "containerized systems are hell")
    (("dj_zmx" . "fair enough") "yellowberryhn" .
     "the music is incredible")
    (("leadengin" . "but do you BOOST TSOOB or TSOOB BOOST?")
     "yellowberryhn" . "what is this game tho")
    (("yellowberryhn" . "what is this game tho") "yellowberryhn" .
     "how did you open that menu")
    (("yellowberryhn" . "how did you open that menu") "yellowberryhn" .
     "oh i see, you can't turn in first person")
    (("eudemoniac" . "present unpacking lcolonPog") "yellowberryhn" .
     "1000% agree")
    (("kuromaruoniisan" .
      "https://www.youtube.com/watch?v=edI3Rev9Kqc&list=FLQlbFMjradCZfUqJqC5kZGg&index=18")
     "yellowberryhn" . "the operation of a tool")
    (("kuromaruoniisan" . "Binchiling?") "yellowberryhn" .
     "i'm full of geometric shapes too")
    (("liquidcake1" . "Blorgans, perhaps?") "yellowberryhn" . "exactly")
    (("kuromaruoniisan" . "we're just full of non-Euclidian shapes")
     "yellowberryhn" . "we're making a new file")
    (("djkawaiifieri" . "!lurk") "yellowberryhn" .
     "this is a huge advancement")
    (("liquidcake1" . "Elona!") "yellowberryhn" .
     "i think people should stream so little that it wraps around and overflows and people start to stream endlessly")))

(defconst w/dna-sampie159
  '((("mxowlex" .
      "90C6/B5/G5/D5/B/C5/D5/G5/C6/B5/G5/D5/B/C5/D5/B/A5/G5/A5/G5/B/C5/D5/G5/A5/G5/A5/G5/B/C5/D5/C5|A3/E4/A4~~~~/F4/E4/C4/A3/E4/A4~~~~/F4/E4/C4/F3/C4/F4~~~~/D4/C4/B3/F3/C4/[F4D4~~~~]/D4/C4/B3|C5~~/B4~~/////////////////////G4~~/B4~~/A4~~/////////A4~~")
     "sampie159" .
     "first person to pronounce my username as i intended boxPog")
    (("tomaterr" . "yes") "sampie159" . "everyone else calls me sampee")
    (("tomaterr" . "sampy") "sampie159" . "now back to lurking")
    (("nikolaradhristov" .
      "@a_tension_span my day has been ruined and my dissatisfaction is immeasurable")
     "sampie159" . "so talented")
    (("zullfix_" . "o7") "sampie159" . "just a silly little alphabet")
    (("dogwithglasseson" . "thanks son") "sampie159" .
     "go clean your room clonq")
    (("mickynoon" .
      "this is a shareholder meeting now for fucking real holy shit")
     "sampie159" . "makes sense so far")
    (("liquidcake1" . "Verisimilitude.") "sampie159" . "Vendetta")
    (("ellg" . "its a bsd if you squint at it") "sampie159" .
     "@TackVector i don't blame you")
    (("asquared31415" .
      "you unfortunately cannot actually send CR or LF over twitch, due to Reasons")
     "sampie159" . "boxKEK")
    (("ellg" .
      "the eventsub message is like 200 lines of json per message lol")
     "sampie159" .
     "would be funny to see the filled with \"The Computer\"")
    (("klingonne" . "i'll let you touch my d") "sampie159" .
     "i wonder if jai will be available to the public until 2030")
    (("imgeiser" . "@mickynoon I'll rip your nuts apart >:(") "sampie159"
     . "don't listen to clonk, he's just text on a screen")
    (("prodzpod" .
      "its stuff like rocks, like the feeling of mining rocks, mining rocks on ableton live with that blocky ass waveform on 0.6 db")
     "sampie159" . "stay up late kiddos")
    (("bezelea" . "f-base or something") "sampie159" .
     "alright chatters it's been a good lurk but imma go the gym now")
    (("stefisot" . "where is modclonk, is it well?") "sampie159" .
     "what a nice surprise stream is still going")
    (("ellg" . "did that play music too") "sampie159" .
     "critical hit would startle me so much")
    (("mickynoon" . "someone said irc ?") "sampie159" . "IRC ?")
    (("eudemoniac" . "@Faeliore AINTNOWAY") "sampie159" .
     "I cant vote for some reason but arch btw")))

(defconst w/dna-a_tension_span
  '((("hazmatvt" . "Hey Dj!") "a_tension_span" .
     "In my experience, all good managers are  those that don't have \"manager\" as their assigned role. Meaning people who have some job within your project but are also managing things on the side. You need to be lucky and have someone who can fit into the team dynamic like that though.")
    (("tyumici" .
      "Like, idk how to shift off my senior dev stuff to the jr and mid devs")
     "a_tension_span" .
     "But when there is someone like that they are so much better than \"pure\" managers (in my experience)")
    (("agpen" . "very true") "a_tension_span" .
     "Can it also be keyboard controlled, please?")
    (("leadengin" . "!resolution Return to Developer") "a_tension_span" .
     "Can we have vi keybindings in the emacs-kanban board?")
    (("a_tension_span" .
      "Can we have vi keybindings in the emacs-kanban board?")
     "a_tension_span" . "Hi!")
    (("liquidcake1" . "OUB") "a_tension_span" . "sure")
    (("yellowberryhn" . "!resolution stream once a week")
     "a_tension_span" .
     "@imgeiser Oh god, I'm sorry you have to go through this. If you need a virtual shoulder to cry one, we're all here for you.")
    (("imgeiser" . "WAKE UP") "a_tension_span" .
     "C is my hometown, but writing C on windows sounds about as pleasing as removing ones fingernails.")
    (("wina" . "ratJAM") "a_tension_span" . "yanderaeJams")
    (("eudemoniac" . "geegaWiggly lcolonWiggly geegaWiggly lcolonWiggly")
     "a_tension_span" . "yanderaeCheer yanderaeCheer freamRave")
    (("steeledshield" . "please") "a_tension_span" . "We'll hear*")
    (("imgeiser" . "500 bpm...") "a_tension_span" .
     "@dj_zmx It is bad, but it is not the worst thing about packaging nowadays.")
    (("saferq" . "boosting makes your number go up") "a_tension_span" .
     "Not meating deadlines is a necessity in gamedev, otherwise you're not actually developing a game.")
    (("yellowberryhn" . "exactly") "a_tension_span" .
     "That's not a short story, that actually happened to you! Don't you remember?")
    (("imgeiser" . "I") "a_tension_span" .
     "I've seen songs of syx so much scroll by but never actually checked it out")
    (("celeste_kyra" .
      "I also have another size monitor, if you want that resolution too.")
     "a_tension_span" .
     "The kind of game that makes you say \"Huh, I could also spend my brain energy on doing actual programming for one of my 1000 open projects\"")
    (("a_tension_span" .
      "The kind of game that makes you say \"Huh, I could also spend my brain energy on doing actual programming for one of my 1000 open projects\"")
     "a_tension_span" . "aka every zacktronic game")
    (("yellowberryhn" .
      "zachtronics games are crazy hard but a good time")
     "a_tension_span" . "2014")
    (("awichan_" .
      "i just tried but i used the wrong sugar and the sauce burned :(")
     "a_tension_span" .
     "It feels like Go is beyond its peak already though. Maybe that's just my selection bias but the comments I've seen about Go have become more negative than positive over the past 3 or so years.")
    (("zulleyy3" . "Go is Kubernetes") "a_tension_span" .
     "Hm, not sure, Go and Rust seem like very distinct crowds")))

(defconst w/dna-faeliore
  '((("emoster_again" . "dillio7PikaRoll dillio7PikaRoll dillio7PikaRoll")
     "faeliore" . "Ayo")
    (("emoster_again" . "how am i supposed to pursue idol dream now")
     "faeliore" . "VS Bald")
    (("fn_lumi" . "badcop3Bonk") "faeliore" .
     "Didnt even install vs code for the meme")
    (("anokayguy0" . "HAPPY NEW CLONQ") "faeliore" . "Happy GNU year")
    (("codespace0x25" . "I used ed") "faeliore" . "Nano is peak")
    (("steeledshield" . "Kine") "faeliore" . "!resolution 2560x1440")
    (("deep_field" . "Whoa. You sound different") "faeliore" .
     "skinwalker skinwalker")
    (("setolyx" . "@a_tension_span oh? didn't know this") "faeliore" .
     "its the ultimate bait")
    (("m18m18m18" .
      "Load average indicates the number of processes in runnable state over that past minute on average, or something like that.")
     "faeliore" . "its the new twitch meta")
    (("maxusspielt" . "!resolution Exercise :L") "faeliore" .
     "at some point you're gonna need a reference manual for new people to understand all the stuff they can do on your stream")
    (("a_tension_span" . "add \"raw\" to search") "faeliore" .
     "you want one of the ancient PDF ones")
    (("prodzpod" . "this is a good *notices css* \"thats enough\" moment")
     "faeliore" . "nana")
    (("prodzpod" . "tsoot?") "faeliore" . "go for dinosaur onesie")
    (("yiffweed" .
      "Can Lain cosplay include that like thin dress while wrapped in tons of cables?")
     "faeliore" .
     "my wife and I did a konosuba cosplay at a convention, it was fun")
    (("abipolarcarp123" . "I know what you mean") "faeliore" .
     "oh yeah I got one of those from work and they're insanely huge")
    (("dfluxstreams" . "brutal") "faeliore" .
     "I went back to work after a long christmas break today and this is giving me flashbacks to the horror of my kanban board this morning")
    (("dfluxstreams" . "whats that like?") "faeliore" . "its cool")
    (("setolyx" . "bogaP") "faeliore" .
     "yeah you're probably gonna have to import MobX")))

(defconst w/dna-liquidcake1
  '((("morgvn_" . "no he's still making low poly gun") "liquidcake1" .
     "The trick is to type nonsense and press enter and see if you get an error.")
    (("kuromaruoniisan" . "Heyo") "liquidcake1" . "!resolution 3840x2160")
    (("kuromaruoniisan" .
      "I thought of you while wrangling Unity shaders")
     "liquidcake1" . "You're live on 42\".")
    (("remlessthanthree" . "nah man we bop to music together")
     "liquidcake1" .
     "Having other people listen to my music always makes me cringe.")
    (("djkawaiifieri" . "Strimmer, play Foo Fighters strimmer")
     "liquidcake1" .
     "!resolution Stream at least one probably cringe live stream this year.")
    (("modclonk" . "@OlgaOkami Hi! Happy New Year!! :D") "liquidcake1" .
     "Oh no, is Friend going to be taunting us?")
    (("remlessthanthree" .
      "I am sooooo confused by these naming conventions")
     "liquidcake1" . "Have you ever broken a website terms of use?")
    (("drawthatredstone" . "lcolonGreen") "liquidcake1" . "lcolonGreen")
    (("crazykitty357" . "lcolonGreen [i](this was sent from godot)[/i]")
     "liquidcake1" .
     "Turns out everyone thought you were just constantly flexing your copious green bills.")
    (("djkawaiifieri" .
      "That genuinely was one of my favorite streams of hers")
     "liquidcake1" . "Skateboarding games are very good for DMCA.")
    (("djkawaiifieri" .
      "DMCA is a myth. You barely get muted in the vod and you're not going to get copyright claimed for a licensed ost game")
     "liquidcake1" .
     "Yeah, they're full of \"pop\" music like Papa Roach.")
    (("spooksyvt" .
      "lcolonNoted lcolonNoted lcolonNoted lcolonNoted lcolonNoted lcolonNoted lcolonNoted lcolonNoted lcolonNoted lcolonNoted lcolonNoted lcolonNoted lcolonNoted lcolonNoted lcolonNoted lcolonNoted lcolonNoted")
     "liquidcake1" .
     "@DJKawaiiFieri Just don't ever play anything with Star Wars music in it...")
    (("azrhyga" . "The best gift in the world ever created")
     "liquidcake1" . "Merry New Christmas")
    (("holy_cow_420" . "!resolution Realize the NPC within")
     "liquidcake1" . "Oh no.")
    (("kuromaruoniisan" .
      "an option is a right to sell/buy a stock at a specific price")
     "liquidcake1" .
     "The best gifts are those that take. Like a noisy train set to take away the parents' peace.")
    (("olgaokami" . "lack of good horrors is real...") "liquidcake1" .
     "Like Resident Evil?")
    (("kuromaruoniisan" . "I recently re-watched 5th element")
     "liquidcake1" .
     "The most memorable movie I watched last year was the Star Wars Christmas Special.")
    (("kuromaruoniisan" . "!resolution 800x600") "liquidcake1" .
     "I used to be able to watch movies, and now, yeah, they just feel like a series of scenes, rather than something coherent?")
    (("stuxvt" . "!resolution 69x420") "liquidcake1" .
     "@KuromaruOniisan That thumps.")
    (("zulleyy3" . "It's very much about \"don't do wasteful things\"")
     "liquidcake1" .
     "Agile manifesto is \"do things that make sense\", but many implementations are \"do things that seem from the outset to make sense, but then do them to a nonsensical degree\".")))

(defconst w/dna-vettle
  '((("codespace0x25" . "play stardew") "vettle" .
     "First clonk gaming stream will be the doom port")
    (("codespace0x25" . "community games") "vettle" . "Babyclonk?")
    (("liquidcake1" . "I heard baby showers were pretty dangerous.")
     "vettle" . "God these puns lmao")
    (("emoster_again" . "thats awesome") "vettle" .
     "Also glad to hear modclonks cackles in between the unhinged statements")
    (("acher0_" . "LCOLONQ LURKERS") "vettle" .
     "Brother you need a real PC")
    (("steeledshield" . "2013") "vettle" .
     "Did uno on xbox get overtaken by pedophiles?")
    (("darpsyx" . "i was 13 btw") "vettle" .
     "Dont worry its a deterministic game")
    (("kinwoop" .
      "Development under restrictions always results in neato stuff")
     "vettle" . "Game dev in general is super cool")
    (("abipolarcarp123" . "helo clonk") "vettle" .
     "@Kinwoop Did you pull up porn to make him proud?")
    (("modclonk" . "@liquidcake1 Y2K stuff is interesting to me")
     "vettle" . "Lmao Y2K checks")
    (("vettle" . "Lmao Y2K checks") "vettle" . "What a wild time")
    (("digbycat" . "lol Y2K") "vettle" . "Are you guys ready for 2038?")
    (("a_tension_span" . "@Wina Yes") "vettle" . "Im ready for Y238K")
    (("a_tension_span" . "@GenDude Agreed.") "vettle" .
     "I got my tinfoil already prepared, just need the doomsday bunker to be built")
    (("liquidcake1" .
      "Experimental games were bad at the time but good because they broke ground. I think those are maybe \"bad\" now.")
     "vettle" . "Classic WoW was such a MASSIVE grind")
    (("kinwoop" . "that we all wake from") "vettle" . "I enjoy FFXIV")
    (("codespace0x25" . "boop") "vettle" .
     "As I age I'm happy that FFXIV treats me as an adult and doesnt force me to grind bullshit")
    (("wina" .
      "ngl the thing that makes wow be cool to me is how interesting the community is. the game is just an excuse for this people to do funny things. The reddit posts narrating the entire timeline are deserving of full fledged documentaries lol https://old.reddit.com/r/HobbyDrama/comments/riq4fq/games_world_of_warcraft_part_1_beta_and_vanilla/")
     "vettle" . "I just like doing the fights")
    (("danktownbunny" .
      "FF14 was never an mmo, it's just an online final fantasy jrpg imo")
     "vettle" . "I do week 1 savage and ultis with the homies")
    (("darpsyx" . "@darpsyx oh i understand yeah") "vettle" .
     "If you ever want to do more hit me up I'm down to help out folks")))

(defconst w/dna-a_tension_span
  '((("hazmatvt" . "Hey Dj!") "a_tension_span" .
     "In my experience, all good managers are  those that don't have \"manager\" as their assigned role. Meaning people who have some job within your project but are also managing things on the side. You need to be lucky and have someone who can fit into the team dynamic like that though.")
    (("tyumici" .
      "Like, idk how to shift off my senior dev stuff to the jr and mid devs")
     "a_tension_span" .
     "But when there is someone like that they are so much better than \"pure\" managers (in my experience)")
    (("agpen" . "very true") "a_tension_span" .
     "Can it also be keyboard controlled, please?")
    (("leadengin" . "!resolution Return to Developer") "a_tension_span" .
     "Can we have vi keybindings in the emacs-kanban board?")
    (("a_tension_span" .
      "Can we have vi keybindings in the emacs-kanban board?")
     "a_tension_span" . "Hi!")
    (("liquidcake1" . "OUB") "a_tension_span" . "sure")
    (("yellowberryhn" . "!resolution stream once a week")
     "a_tension_span" .
     "@imgeiser Oh god, I'm sorry you have to go through this. If you need a virtual shoulder to cry one, we're all here for you.")
    (("imgeiser" . "WAKE UP") "a_tension_span" .
     "C is my hometown, but writing C on windows sounds about as pleasing as removing ones fingernails.")
    (("wina" . "ratJAM") "a_tension_span" . "yanderaeJams")
    (("eudemoniac" . "geegaWiggly lcolonWiggly geegaWiggly lcolonWiggly")
     "a_tension_span" . "yanderaeCheer yanderaeCheer freamRave")
    (("steeledshield" . "please") "a_tension_span" . "We'll hear*")
    (("imgeiser" . "500 bpm...") "a_tension_span" .
     "@dj_zmx It is bad, but it is not the worst thing about packaging nowadays.")
    (("saferq" . "boosting makes your number go up") "a_tension_span" .
     "Not meating deadlines is a necessity in gamedev, otherwise you're not actually developing a game.")
    (("yellowberryhn" . "exactly") "a_tension_span" .
     "That's not a short story, that actually happened to you! Don't you remember?")
    (("imgeiser" . "I") "a_tension_span" .
     "I've seen songs of syx so much scroll by but never actually checked it out")
    (("celeste_kyra" .
      "I also have another size monitor, if you want that resolution too.")
     "a_tension_span" .
     "The kind of game that makes you say \"Huh, I could also spend my brain energy on doing actual programming for one of my 1000 open projects\"")
    (("a_tension_span" .
      "The kind of game that makes you say \"Huh, I could also spend my brain energy on doing actual programming for one of my 1000 open projects\"")
     "a_tension_span" . "aka every zacktronic game")
    (("yellowberryhn" .
      "zachtronics games are crazy hard but a good time")
     "a_tension_span" . "2014")
    (("awichan_" .
      "i just tried but i used the wrong sugar and the sauce burned :(")
     "a_tension_span" .
     "It feels like Go is beyond its peak already though. Maybe that's just my selection bias but the comments I've seen about Go have become more negative than positive over the past 3 or so years.")
    (("zulleyy3" . "Go is Kubernetes") "a_tension_span" .
     "Hm, not sure, Go and Rust seem like very distinct crowds")))

(defconst w/dna-saferq
  '((("a_tension_span" . "sure") "saferq" . "hit us with the OUB")
    (("a_tension_span" .
      "@dj_zmx It is bad, but it is not the worst thing about packaging nowadays.")
     "saferq" . "making python packages is the devil, dont do it")
    (("imgeiser" . "small inconvenience") "saferq" .
     "boosting makes your number go up")
    (("gendude" . "incoming factorIO") "saferq" .
     "Bangers and mash sound good")
    (("modclonk" .
      "@steeledshield https://youtu.be/OMjYXVJDAy8?si=nCcDxt_3GJubou9y")
     "saferq" .
     "Agile is deprecated. Benevolent Dictators For Life is the new best practice.")
    (("gendude" .
      "Y'all ever had them good mashed 'taters? WHOOOO-WEEE thats the good stuff")
     "saferq" .
     "I hereby proclaim you to be an honorary Florida man. You may now use \"y'all\" in formal speech.")
    (("a_tension_span" . "Let's do it") "saferq" .
     "The mask is always truer to the self. Do it.")
    (("mickynoon" .
      "my fav was the file system crash and somehow you still managed to be live")
     "saferq" . "gotta learn to respect the Mongolian Vowel Separator")
    (("nineteenninetyx" . "lets C") "saferq" .
     "There was a time when I thought it was reasonable to learn every function in every module. I am not a smart man.")
    (("destinywaits" . "some sort of aliasing?") "saferq" .
     "One time I intentionally deleted glibc.so.2.0, do I win?")
    (("yukievt" . "yukievRoll yukievRoll yukievRoll") "saferq" .
     "Never had an old oem drive quit on you? Totes jealous")
    (("prodzpod" . "go electric") "saferq" .
     "I do enjoy some Debian. It just werks.")
    (("tomaterr" . "it's archiving the jaunts the stream takes") "saferq"
     . "helix vs kakoune?")
    (("saferq" . "helix vs kakoune?") "saferq" .
     "neovim is too mainstream")
    (("biodigitaljaz" . "How's the magic 8 ball?") "saferq" .
     "damn he really saw this one coming")
    (("scared3cat" . "I did it! My laptop is fixed!") "saferq" .
     "@Scared3Cat Great! Now you get to break it in a new and fun way.")
    (("morgvn_" . "gangnam style") "saferq" .
     "an emote based arg/ctf would destroy my life")
    (("herrington1" .
      "I thought it was how fast the music is but we can't hear it because of copyright")
     "saferq" . "Whats the whats the bpm counter for counter for")
    (("prodzpod" . "the song is also totally about gangstalking")
     "saferq" .
     "If you're a psyop, you have to tell us you're a psyop. Otherwise it's entrapment. That's the law.")
    (("holy_cow_420" . "do you have any fav sea creature?") "saferq" .
     "that really takes me back")))

(defconst w/dna-badcop_
  '((("ginjivitis" .
      "id like to get in on that some time too. it would feel validating for all the hard work")
     "badcop_" . "lcolonNoted")
    (("badcop_" . "lcolonNoted") "badcop_" . "kuvimaNPoggers")
    (("yellowberryhn" . "bacdop") "badcop_" . "tyyyy")
    (("badcop_" . "tyyyy") "badcop_" .
     "i just want partner so i can get rid of my underscore LUL")
    (("badcop_" .
      "i just want partner so i can get rid of my underscore LUL")
     "badcop_" . "that's the dream")
    (("destinywaits" . "kanban board but emacs?") "badcop_" .
     "can u ban kanban")
    (("moonszol" . "tmux?") "badcop_" . "emux")
    (("abipolarcarp123" . "A curious proposition arises") "badcop_" .
     "perchance badcop3Thinking")
    (("bezelea" . "you cant just say perchance") "badcop_" .
     "@bezelea perchance i can")
    (("badcop_" . "@bezelea perchance i can") "badcop_" . "indubitably")
    (("fn_lumi" . "badcop3LUL") "badcop_" . "per se per say")
    (("prodzpod" . "per oxide..") "badcop_" . "per diem")
    (("destinywaits" . "i am perturbed") "badcop_" .
     "people using arrow keys will always amaze me")
    (("bezelea" . "it doesn't take too long to learn it") "badcop_" .
     "tomorrow rincsCRAFjam")
    (("docmaho" .
      "A3/A3c4//A4//D4//d4/////A3/A3c4//A3//f4//D4/////A3/A3A4//g4//D4//d4//c4//G4/G4g4//D4//f4//D4///////////")
     "badcop_" . "im not old yet")
    (("badcop_" . "im not old yet") "badcop_" . "close enough LUL")
    (("deep_field" .
      "I remember some kind of adventure game in an text editor that taught it but could never find it again")
     "badcop_" .
     "omg im definitely making happy birthday with note blocks tomorrow you inspired me")
    (("deep_field" . "@DestinyWaits Thats it! thank you!") "badcop_" .
     "@lcolonq should i stream the minecraftening or no i cant decide")
    (("docmaho" . "@badcop_ Do you need notesheets for the crafting?")
     "badcop_" . "lcolonNodders")
    (("prodzpod" .
      "clonk \"i would stream (minecraft (video game (on stream)))\" :gasp:")
     "badcop_" . "many such questions")))

(defconst w/dna-vesdev
  '((("Azrhyga" . "Bone showcase confirmed?") "vesdev" . "Joel")
    (("prodzpod" . "lcolonq style right") "vesdev" . "the people of lerp")
    (("RayMarch" . "freya acegikmo holmer") "vesdev" . "wtf")
    (("badcop_" . "nobody tells me anything") "vesdev" . "not a 3rd one")
    (("vesdev" . "not a 3rd one") "vesdev" . "please")
    (("forsen" . "forsenWhat  forsenKek") "vesdev" . "NOOOO")
    (("Tomaterr" . "no fuckin godot support?") "vesdev" .
     "ok yea nvm saved")
    (("PracticalNPC" . "godot gang") "vesdev" . "lmao")
    (("wyndupboy" .
      "this gamejam seems like an extended ad for unity and unreal.")
     "vesdev" . "https://badcop.itch.io/caverim")
    (("static_anachromatic" . "...what") "vesdev" .
     "computer hacking kuvimaNKek")
    (("fcollector" .
      "Did it say no potentially dangerous activities too, like the YouTube TOS that technically bans 90% of the platform?")
     "vesdev" . "jaywalking simulator")
    (("goofysystem" . "does the primeagen know you haskell?") "vesdev" .
     "laravelq")
    (("chocojax" .
      "holy frick that takes out like 99% of the games out there!!")
     "vesdev" . "just make a game that scrolls the rules of this jam")
    (("BoganBits" . "I want a Real Life wiki") "vesdev" .
     "warframe mentioned")))

(defconst w/dna-kierem__
  '((("Tyumici" .
      "Tune in next time for more high-yield entertainment!")
     "kierem__" . "Joel")
    (("Tyumici" . "lcolonHi") "kierem__" . "o/")
    (("eudemoniac" . "Joel") "kierem__" . "Joel")
    (("[VOICE]" .
      "6. Hello, hello, uh, who, who else? Who else is on here? Who, who'd they put on this machine? This infernal de-")
     "kierem__" . "lcolonSpin lcolonSpin lcolonSpin")
    (("yiffweed" . "Waiting room for Path of Exile") "kierem__" . "plink")
    (("Zullfix_" . "Listening") "kierem__" . "...")
    (("[VOICE]" .
      "fun. Right? We're trying to identify some fun. I have some ideas. I think probably we're going to fucking write some emacs lists and do some other things besides...")
     "kierem__" . "Joeling")
    (("[VOICE]" .
      "You got the wood out? Yeah, just make an MMORPG tonight. You're literally streaming video with a video camera. All that's needed now is game and we have video game. Yeah. ")
     "kierem__" . "Joel")
    (("[VOICE]" .
      "This is how, so IKR13, it's sort of like this, hello abc123, thank you for 75 also. So I'm not using the keyboard lg because we spore")
     "kierem__" . "Joel")
    (("LCOLONQ" .
      "#cyberspace on IRC at colonq.computer:26697 (over TLS)")
     "kierem__" . "jol")
    (("[VOICE]" .
      "and we just write like a native Raylib game. Like it doesn't actually display, it runs Emacs in bash mode, just like displays it headless and like creates another X window.")
     "kierem__" . "Joel")
    (("stoicmana" . "the yapping hour") "kierem__" . "GoldenJoel")
    (("boga_14" . "MODCLONK") "kierem__" . "friend is dead lcolonSadge")
    (("archible" . "LETSGO") "kierem__" . "o/")
    (("krzysckh" . "can you please show you r id please oh please")
     "kierem__" . "lcolonHi")
    (("resxnance_live" . "I was here but it is hilarious xd") "kierem__"
     . "Joelest")
    (("ellg" . "lmao") "kierem__" . "this is so cool omg lcolonShine")
    (("Tyumici" . "RUINED") "kierem__" . "oh no")
    (("wyndupboy" . "(but not as bad as you think)") "kierem__" . "2")
    (("LCOLONQ" . "Joel") "kierem__" . "Joel")
    (("a_tension_span" .
      "The breaking of the hand cam needs to be recorded as lore event in the glossary btw, @prodzpod")
     "kierem__" . "Joel")
    (("[VOICE]" .
      "Oh my god. Oh my god. The quality is excellent voice. Thank you. Um, the the curse of raw raw all things")
     "kierem__" . "Joeler")
    (("resxnance_live" . "literally feels like ad yo hahha") "kierem__" .
     "...")
    (("Venorrak" . "hi jake lcolonHi") "kierem__" . "lcolonQ")
    (("KotaruComplex" . "could you mr. beastify a chat message?")
     "kierem__" . "floor")
    (("aliant2" . "actually froze") "kierem__" . "...")
    (("[VOICE]" . "wait literally leaving the room") "kierem__" . "Joel")
    (("gtfrvz" . "need EMERGENCY MITTENS?") "kierem__" . "o/")
    (("[VOICE]" .
      "It wasn't like, you know, Soldiers normally can take weeks and weeks and weeks. You're playing Hearts of Iron 4 right now, the game has a themed wiki browser in it. Yeah, I know-")
     "kierem__" . "Joel")))

(defconst w/dna-octorinski
  '((("Tomaterr" . "@MODCLONK based") "octorinski" .
     "Hello computer and all Joel")
    (("mickynoon" . "Joel") "octorinski" . "I just came in :)")
    (("loweffortzzz" . "based hair") "octorinski" . "lcolonHi")
    (("XorXavier" . "live chatters") "octorinski" . "lcolonSoTrue")
    (("steeledshield" . "pacamn?") "octorinski" . "plink")
    (("[VOICE]" .
      "that has a cool little custom display slash controller thing it's kind of it's kind of interesting interlaced oh yes")
     "octorinski" .
     "Looks cool from what I can see https://static.arcade-game-sales.com/images/products/large/17952_2.jpg")
    (("wyndupboy" . "that looks like something from General Atomics.")
     "octorinski" . "ProxMox is a KVM hypervisor")
    (("[FRIEND]" .
      "ooh a yellow dot, what's the universe up to? probably just stretching its legs!")
     "octorinski" . "It uses libvirt under the hood")
    (("[VOICE]" .
      "I see I'm concerned about all of this the how is the audio by the way is the audio tolerable are we")
     "octorinski" . "Very tolerable")
    (("Polars_Bear" . "who is talking in the background?") "octorinski" .
     "(we (like (LISP)))")
    (("[VOICE]" .
      "In VTuber circles, they might call this an off-collab. I think that's today.")
     "octorinski" . "bpm_counter++")
    (("[VOICE]" .
      "like the collection of papers submitted to a conference the the previous the previous fuck fuck")
     "octorinski" . "submissions?")
    (("Tomaterr" . "the backlog") "octorinski" . "Itinerary?")
    (("[VOICE]" .
      "is a this is it's it's quite it's quite scary yeah just throw coins directly at the back")
     "octorinski" . "ACTION Octorinski was permanently banned")
    (("eudemoniac" . "good bit") "octorinski" .
     "@dehidehiNotFromFinland BROTHER NOOO")
    (("[VOICE]" .
      "I'm still the verdict is still out on that topic. I would say the You know all sorts of all sorts of ghosts")
     "octorinski" . "deja vu is crazy though")
    (("[VOICE]" . "Oh, I see I see") "octorinski" . "EntireShrekMovie")
    (("dehidehiNotFromFinland" .
      "IP leaked, hotel network nuked. Thanks obama")
     "octorinski" . "1 tenth of a second CarlSmile")
    (("[VOICE]" .
      "Which is kind of tricky over. I don't know if you actually do that the Maybe you can")
     "octorinski" . "hi")
    (("[VOICE]" .
      "IRC is a new line delimited protocol, right? So I don't know if you can actually send a new line within an IRC message, but I think there are.")
     "octorinski" . "Didn't work :(")
    (("[VOICE]" .
      "I'm still, I'm still lacking, I'm still lacking visibility here.")
     "octorinski" . "I tried to newline")
    (("[FRIEND]" .
      "wow, yellowberryhn, you're swimming right through the copfish collection!")
     "octorinski" . "Lcolonq looks like")
    (("dehidehiNotFromFinland" . "You are rob boss?") "octorinski" .
     "Jason Momoa was also Aquaman")
    (("rudle" . "car gone") "octorinski" . "cargo run? nah. cargo vroom")
    (("[VOICE]" .
      "And we need to git add everything. We need to add everything. Linking with CC failed, mentioned LFG.")
     "octorinski" . "@yellowberryHN RareParrot")
    (("[VOICE]" .
      "the moment I would say yeah we're experiencing that's delay yeah that's delay that's a rust analyzer moment that's death I would")
     "octorinski" . "real latent stremer")
    (("[VOICE]" .
      "It's an extruded. I don't know. It's got four legs. It's got udders. Maybe if it's if it's a cow")
     "octorinski" . "Joel")
    (("chixie9901" . "Cow2DProjectedDotJpeg") "octorinski" .
     "Graphics design is my passion and what you have is the best I could attempt")
    (("[VOICE]" .
      "decompiler god damn I like it though I you know you know you can go a long way with art if you just give everything")
     "octorinski" . "That cow has personality")
    (("loweffortzzz" .
      "it doesnt have to look exactly like a cow its rounded")
     "octorinski" . ">:^3")
    (("Tomaterr" . "nicheCow") "octorinski" . "stripCool nice cow")
    (("[VOICE]" .
      "Okay, okay, tension span. What is this? What is this? What are you saying? Clown? Clown?")
     "octorinski" . "@loweffortzzz lcolonGreen")))

(defconst w/dna-charleyfolds
  '((("xivandroid" . "Wassup") "charleyfolds" .
     "I wish I was a rotating pikachu")
    (("yukievt" . "C/////|/D////|//E///|///F//|////G/|/////A")
     "charleyfolds" . "This is my second time using it")
    (("charleyfolds" . "This is my second time using it") "charleyfolds"
     . "I know what it does >:(")
    (("prodzpod" . "can you only refund the most recent redeem")
     "charleyfolds" . "It's good really, I'll save for a clone")
    (("gendude" . "lcolonRaid") "charleyfolds" .
     "VoHiYo VoHiYo charle236Takethis charle236Takethis charle236Takethis charle236Takethis charle236Takethis")
    (("a_tension_span" . "Sorry, have sleepy brain") "charleyfolds" .
     "It was good, just doodlin' and a little noodlin'")
    (("codespace0x25" . "h") "charleyfolds" . "LgoslingQ")
    (("mickynoon" . "lonely stack overflows in your area") "charleyfolds"
     .
     "I'm letting life hit me until it gets tired. Then I'll hit back. It's a classic rope-a-dope.")
    (("kinwoop" . "as its all timelines") "charleyfolds" .
     "To be honest, when I found out the patriarchy wasn't just about horses, I lost interest.")
    (("ginjivitis" . "sounds brisk") "charleyfolds" .
     "The whole town's underwater. You're grabbing a bucket when you should be grabbing a bathing suit.")
    (("vrkitect" . "arthur262Sealcry") "charleyfolds" .
     "Those are nice, huh? And they're not real, so they'll last forever.")
    (("akashicmagick" .
      "Trying to think about a clever pun thats computer related play on words for like harry potter's sorting hat")
     "charleyfolds" . "I drive.")
    (("codespace0x25" . "earendFlip earendTable") "charleyfolds" .
     "You want a why. Well, maybe there isn't one. Maybe this is just something that happened.")
    (("setolyx" . "I love the idea of this so much.") "charleyfolds" .
     "Let me put it this way. I'm standing in front of a burning house, and I'm offering you fire insurance on it.")
    (("acher0_" . "olga is day one huh") "charleyfolds" .
     "You have to believe that life is more than the sum of its parts. What if you can't put the pieces together in the first place?")
    (("retroboi128thegamedev" . "Hello, World! And Hello, Chat!")
     "charleyfolds" .
     "Sometimes I think that the one thing I love most about being an adult is the right to buy candy whenever and wherever I want.")
    (("khlorghaal" .
      "im like \"whoa this underpass doesnt have used syringes?!\"")
     "charleyfolds" .
     "It was important for me to get an outside look at America even though I grew up in Canada, it's an incredible country and I love it, but it's so close. It's like being too close to a Monet or something.")
    (("eudemoniac" . "boss will get tired and let friend run the stream")
     "charleyfolds" . "I was highly influenced by violence.")
    (("piglilith" .
      "LCOLONQ please remember to turn your computer off on december 31, 1999")
     "charleyfolds" .
     "Cars can have a hypnotic effect. You can get in a car and get out and not really remember the trip.")
    (("khargoosh" . "Oh look. Now he's horny.") "charleyfolds" .
     "There's a hundred-thousand streets in this city. You don't need to know the route. You give me a time and a place, I give you a five minute window. Anything happens in that five minutes and I'm yours. No matter what. Anything happens a minute either side of that and you're on your own. Do you understand?")
    (("vettle" . "Sanest emacs user") "charleyfolds" .
     "My uncle was an Elvis impersonator - his name was Perry, and he went by 'Elvis Perry'.")
    (("a_tension_span" .
      "Wait, who is playing rn? I missed the beginning cause I was in the shower")
     "charleyfolds" .
     "Does the label \"long-term long-distance low-commitment casual girlfriend\" mean nothing to her?")
    (("nichepenguin" .
      "Germans from Germany, Long Germany German, Second German Germany. I am German because my Germany.")
     "charleyfolds" .
     "If you ask me, the devil makes more sense than God does. I can at least see why people would want him around. It's good to have somebody to blame for the bad stuff they do.")
    (("liquidcake1" . "Please put your shoe on your head.")
     "charleyfolds" . "i drive")
    (("faeliore" . "it did lag you real bad, but no crash")
     "charleyfolds" . "Freedom is such a gift.")
    (("danktownbunny" .
      "\"look how easy it is to hack this game, buy it now!\"")
     "charleyfolds" . "[pointing at dog] Is it real?")
    (("lcolonq" . "https://discord.gg/f4JTbgN7St") "charleyfolds" .
     "I sometimes forget to have breakfast in the morning, but when I actually buy a box of cereal, I will probably eat it not only for breakfast but also as a snack later on.")
    (("hardcorexhunter" .
      "mortuary assistant but all the dead bodies are replaced with vtubers")
     "charleyfolds" .
     "For now, I'm just going to keep doing the work and hope I don't get fired.")
    (("daiyadiamandis" . "fun") "charleyfolds" .
     "I feel like it reminded me of a video game, you know?")
    (("kukukaeruvt" . "I drive") "charleyfolds" . "I drive")
    (("running_out_of_unames" .
      "What are the names for those Russians who insist that computer are not Turing machines because of the lack of \"infinite tape\"? They are the ones that have a long \"argument\" with Moskovakis about definition of equality of algorithms")
     "charleyfolds" . "Changing it up and we played tekken")
    (("nineteenninetyx" .
      "see prime intellect solves that by using spooky action at a distance to consume the universe to automatically extend the address space")
     "charleyfolds" . "WAIT no")
    (("running_out_of_unames" .
      "@rondDev Perhaps? But I think they don't agree.")
     "charleyfolds" .
     "To watch a master work at anything is a privilege.")
    (("buddyspizza" . "rails is legacy now") "charleyfolds" . "I drive")
    (("chromosundrift" . "clojure is a decent lisp") "charleyfolds" .
     "If I eat a huge meal and I can get the girl to rub my belly, I think that's about as romantic as I can think of.")
    (("jonkero" . "make mr green a equipable which boosts your stats")
     "charleyfolds" . "I'd like to try thank one person properly")
    (("prodzpod" .
      "all the callback speech only to defall into the abyss that is await...")
     "charleyfolds" . "I drive")
    (("modclonk" . "lcolonHi") "charleyfolds" . "I drive")))

(defconst w/dna-chixie9901
  '((("agpen" . "you're a soyboy now") "chixie9901"
     . "Hello, was browsing software section on twitch and saw you. your model is pretty cool")
    (("imgeiser" . "I know the next troll >:)") "chixie9901"
     . "what are ya working on btw")
    (("regularamoeba" . "cum?") "chixie9901"
     . "ngl when i see code most of the time, feels like im seeing hieroglyphics lol.")
    (("neunmalelf" . "ok TX") "chixie9901"
     . "oh dont worry, im doing a masters in Data science. learning a lot of it rn. going through python a lot for now")
    (("ellg" . "no mods no poker") "chixie9901" . "Neutrinos")
    (("ellg" . "oh i thought that was part of the actual game nevermind")
     "chixie9901" . "fancy to see civi here, sup!")
    (("tomaterr" . "AHAHAHAHAHA") "chixie9901"
     . "Explain Classes in a easy way.")
    (("ellg" . "got a framed picture of rms in a hottub") "chixie9901"
     . "i cant hear anything lol xP")
    (("djkawaiifieri" . "Holy shit") "chixie9901"
     . "btw do you visualize everything you code in your head")
    (("neunmalelf"
      . "as long as you can laugh together - live is good :)")
     "chixie9901"
     . "i thought you were saying \"Mon-clon\" all this time")
    (("prof_anime"
      . "Candy sustains the soul. However, the body requires other resources.")
     "chixie9901"
     . "well its getting late for me. was a nice stream , hope ya have a great one. cya!")
    (("crane0001" . "I read so much shitty manga I love love love it")
     "chixie9901" . "Hello, i like how u say my full twitch name lol xP")
    (("digi_shell" . "@prodzpod digish1Pat") "chixie9901"
     . "Is this a dynamic typed language?")
    (("arenet" . "society opinions yup") "chixie9901"
     . "Good and evil is subjective")
    (("rahnmu" . "Crunk LUL") "chixie9901"
     . "This reminds me of a manga i was reading the other day, called \"Kingdom\". In it there was a part where Kan pishi , a scholar asked a general on what is the true nature of Man.  Was pretty cool")
    (("raditz1504" . "gxauraDance gxauraDance gxauraDance") "chixie9901"
     . "I studied maths for my engineering. Although i didnt like it back then, because i could not visualise it. Thats why videos from 3blue1brown are good, cos it hits your brain just right.")
    (("arenet" . "digish1Wiggly dyingd3Wiggly digish1Wiggly")
     "chixie9901"
     . "@a_tension_span fr fr. He goated. Love his videos on regression, probability, bayes theorem, etc")
    (("imgeiser" . "yea") "chixie9901"
     . "What do ya think of  split ergo keebs?")
    (("a_tension_span" . "bless smulchYblessed") "chixie9901"
     . "Wat were we doing again?")
    (("prodzpod" . "mhm") "chixie9901"
     . "Got a question, since im new to IT field and all. Does your imposter syndrome ever go away?")))

(defconst w/dna-jddoesdev
  '((("prodzpod" . "we dont really code") "jddoesdev" . "hey hey")
    (("jddoesdev" . "hey hey") "jddoesdev" . "thanks for the SO")
    (("prodzpod" . "thats not functional ... ... ... ...") "jddoesdev"
     . "yellow push it real good")
    (("ryanwinchester_"
      . "your hair is a bear trying to get into my compose bin")
     "jddoesdev" . "red oh. push it.")
    (("codespace0x25" . "what should I 3D Print") "jddoesdev"
     . "Are you starting an OnlyFlows?")
    (("tyumici" . "the anti-clonk") "jddoesdev" . "sus")
    (("liquidcake1"
      . "If I had understanding of Web 5.0 or whatever we're on I'd be clipping \"lcolonq uses REAL VOICE\" for the viral YouTube clout right now.")
     "jddoesdev" . "never gonna give you up polyphonic")
    (("gendude" . "MRBEAST") "jddoesdev"
     . "the real clonk was the clonks we made around the way")
    (("khargoosh"
      . "I dont think we should be talkin about peoples super idle tallys... that aint right")
     "jddoesdev"
     . "i think we should get a whacker for the tallies.  maybe a tallywhacker?")
    (("m18m18m18"
      . "Honestly we dont really understand much, we just go by heuristics")
     "jddoesdev" . "test?")
    (("tw1tchface" . "I took calc but I'd not take away any wonder")
     "jddoesdev"
     . "i show up and you're speaking french.  unfollowed.  i'm out")
    (("cranevt"
      . "Legit one moment normal speech, then the moment he went to the whiteboard it was like a magic girl transformation")
     "jddoesdev" . "how you doing")
    (("abipolarcarp123"
      . "Mathematicians are ranked by how nicely they can make zeta and xi symbols")
     "jddoesdev"
     . "it's raining on the snow now which is going to make it especially miserable")
    (("abipolarcarp123" . "Oh no") "jddoesdev"
     . "now instead of 3 feet of powder, it's getting compressed to 1 foot of fuck this")
    (("modclonk" . "@acher0_  lcolonLove") "jddoesdev" . "SOH CAH TOA")
    (("tw1tchface"
      . "eh, FAFSA paying for your first two years of college is sweet")
     "jddoesdev"
     . "i have an associate's degree from a community college")
    (("cranevt"
      . "Community college is mostly for like the base education requirement but some have good programs.")
     "jddoesdev" . "it helped me get into a completely new career")
    (("azrhyga" . "Hi everyone and hi LCOLONQ!!") "jddoesdev"
     . "where i am there are also satellite purdue campuses where students can get their pre-reqs out of the way the transfer to purdue lafayette")
    (("a_tension_span" . "Just the hot tip?") "jddoesdev"
     . "hot tips in your area")
    (("destinywaits" . "win win") "jddoesdev"
     . "I thought that was where I lived, but they're expanding delivery areas")))

(defconst w/dna-tamwile
  '((("hanbunvt" . "we'll get them next year") "tamwile"
     . "a real politician")
    (("badcop_" . "in the shebang sense") "tamwile" . "erc")
    (("eudemoniac" . "@eudemoniac hell yeah lcolonLove") "tamwile"
     . "erc and rcirc")
    (("morgvn_" . "https://youtu.be/qy-HD9upBfE?si=x5SMEGfoWOEWWDy9")
     "tamwile" . "Can you repeat that everyday ? I think i need it")
    (("liquidcake1" . "agent forwarding is OK if you trust the remote.")
     "tamwile" . "I think you can use .ssh/config")
    (("exodrifter_"
      . "thank you clonk those are some very nice words that you are always eloquent at saying")
     "tamwile"
     . "When you're not sick, it's also good to have a nice nap")
    (("buddyspizza"
      . "If it's in version control, you can see when someone does a haxor.")
     "tamwile"
     . "juan benet described ipfs as \"git + bittorrent\", what do you think ?")
    (("chromosundrift"
      . "in practice I wonder how rejected state transitions could be extracted from subsequent states. If this wasn't easy to manage, then you could poison a player by mixing untrusted state into their player history for a given server")
     "tamwile" . "You need a list of trusted nodes, like freenet")
    (("zulleyy3"
      . "Lcolonq went to the far right side of the normal distribution meme")
     "tamwile"
     . "I have a keybind to copy current tab url, then i have a shell alias to open mpv with clipboard, no distraction")
    (("tamwile"
      . "I have a keybind to copy current tab url, then i have a shell alias to open mpv with clipboard, no distraction")
     "tamwile" . "vimium-c")
    (("tamwile" . "vimium-c") "tamwile" . "i use it on firefox")
    (("sandbox_actual"
      . "As far as I understand it, new features are implemented in Servo and then ported into Gecko once stable as the current flow for features getting into Firefox & Gecko.")
     "tamwile" . "what's so cool about ladybird ?")
    (("tamwile" . "what's so cool about ladybird ?") "tamwile"
     . "oh right, kling videos are good learn material")
    (("tamwile" . "oh right, kling videos are good learn material")
     "tamwile"
     . "I think once you have begun compilation, and it stops, you have prompt and you can say keep going even if errors")
    (("tamwile"
      . "I think once you have begun compilation, and it stops, you have prompt and you can say keep going even if errors")
     "tamwile" . "but i don't know how to say that from cmd invocation")
    (("ericalvin" . "D:") "tamwile"
     . "a long time ago, i had an issue with something called kpathsea, don't remember how i fixed it. Tex has too many layers of complexity")
    (("sciants_streams" . "hi friends! hi lcolonq!") "tamwile"
     . "when you kill a biline, it becomes 2 lines")
    (("spikepls" . "!8ball is friend stuck?") "tamwile"
     . "!8ball Will Clonk acquire third dimension one day ?")
    (("lcolonq" . "@SpikePls 8ball says: It's so over") "tamwile"
     . "8ball didn't answer me, it's blocking single thread ?")
    (("lcolonq"
      . "try: \"nc colonq.computer 31340\", if nc doesn't work try ncat or telnet")
     "tamwile"
     . "@Hexavall If it isn't, you just have to change your definition of normality"))
  )

(defconst w/dna-pralkarz
  '((("XorXavier"
      . "hi its me the computer()")
     "pralkarz"
     . "Is the money bag next to a message in the on screen chat representing a clone?")
    (("SlendiDev"
      . "@agpen oh yeah it might be the protection level strict maybe")
     "pralkarz"
     . "I glanced over to my second monitor and read that as \"Too Many Cocks\".")
    (("justchil_l" . "that would be extra cool") "pralkarz"
     . "We had some unwritten rules at work to help us write actually sane and maintainable JavaScript, it was very nice. Avoiding hoisting, preferring singletons over generic classes, strict null safety, and so on.")
    (("[VOICE]"
      . "Hello, windupboyfade, welcome in, hello, hello, hello. So, so, yes. So, so, yes. I want to, I want to clean up the kitchen.")
     "pralkarz"
     . "@RayMarch Hoisting moves variable declarations to the top of the scope, so you can use `const x = y + z;` on line 1 and define `y` and `z` on line 100 and it'd work.")
    (("The0x539" . "I'll post on discord") "pralkarz"
     . "On my way to write a Malbolge -> Brainfuck compiler.")
    (("wyndupboy" . "Modclonk!") "pralkarz" . "HI MODCLONK!!!")
    (("hydoto" . "@anothercommoninternetuser fluid") "pralkarz"
     . "Stop saying \"milligrams\", you're gonna scare him.")
    (("[VOICE]"
      . "This is notable. I think at least has noble intentions. Yes. I was on my on my fifth account before I left it. Yeah, this is This is fearsome")
     "pralkarz" . "The snowflakes can't handle Brazilian Miku. /s")
    (("a_tension_span" . "peggyxPeggyjam MC stream") "pralkarz"
     . "Friendclonk.")
    (("wyndupboy" . "you are still alive!") "pralkarz" . "!oomfie")
    (("mickynoon" . "it's time for RPG") "pralkarz"
     . "IT'S CHEWSDAY INNIT.")
    (("[VOICE]"
      . "like read nothing about games that I'm really really looking forward to or books or movies or shows because")
     "pralkarz"
     . "@ahmednaser2004 Quentin Tarantino approves this message.")
    (("mickynoon" . "kinda complimented low key do") "pralkarz"
     . "SHORTCLONK???")
    (("Colinahscopy_" . "bye maude yarrWave") "pralkarz"
     . "DON'T THROW THE GRILL BACK INTO THE BASEMENT YOU FREAK.")
    (("chromosundrift" . "darius is an auspicious name!") "pralkarz"
     . "Is it Dariuses or Dariusi?")
    (("[VOICE]"
      . "Osama type AI VTuber situation? Is this the case? Is this the case? This is good. We had the same discussion before. I think this is true.")
     "pralkarz"
     . "I like how we have one (1) X in the docket after an hour.")
    (("[VOICE]"
      . "Unless I'm confusing that, yes, yes, this is the case. What did we get up to today? How was your stream? We have one X in the docker after an hour, and that's a leftover from last time, ProudCash.")
     "pralkarz" . "After 3 hours, I mean.")
    (("acher0_" . "did you run with 3'40\" pace?") "pralkarz"
     . "I am, but it won't stop me from poking fun at you. That's my way of expressing love. lcolonLove")
    (("Partycatlol" . "#kissahomie") "pralkarz"
     . "Scrum master? The one who's responsible for cleaning up the scrum?")
    (("[VOICE]"
      . "Oh, yes, we have we have emask lisp The the yeah, at least yap about programming if our stand up would be three")
     "pralkarz"
     . "In agile you'd need to DELIVER VALUE in 4 streams (2 weeks). lcolonNodders"))
  )

(defconst w/dna-ikatono
  '((("[VOICE]"
      . "kind of a coldness rather than a hotness isomorphic if you get what if you get what I'm saying right like you can dislike things right and you can have ideas about how")
     "Ikatono" . "The tools I \"hate\" most are the ones I use most")
    (("[VOICE]"
      . "katono and uh and and chromo sundrift heyo yeah but i just i just feel like it's it's rarely for the best like it's really like i'm not i'm not even talking like in some")
     "Ikatono"
     . "I strongly dislike a lot of things about python but I still use python a lot")
    (("isomorphica" . "Are you running your own ISP yet or not?")
     "Ikatono"
     . "running my NAS has been a great hobby for the past few years")
    (("[VOICE]"
      . "Bot League, which would be- which would actually go insanely crazy. The big yellow and red warning box, the")
     "Ikatono" . "clonknet over HAM radio")
    (("[VOICE]"
      . "If only we could encrypt. If only we could encrypt. Oh wait, no, this is not like this. This is not, this is not like this. This is, this is, she, she told me that. You heard her.")
     "Ikatono" . "yeah that's the big problem")
    (("[VOICE]"
      . "nice I don't it sucks yes it's not like it's like boiling water it's just like like warm water")
     "Ikatono" . "cold water is way more luxurious")
    (("impforhire" . "\"probably\"") "Ikatono"
     . "I thought cigarillos only existed for high schoolers to empty them and fill with weed")
    (("TruppenPanda" . "clonq handing out shots of hot water at cons :D")
     "Ikatono"
     . "tell them \"stick out your hands\" and pour loose tobacco")
    (("[VOICE]"
      . "this this is for others to decide this is for I don't know perhaps the I don't know I don't know how we I don't want to get into that whole situation")
     "Ikatono" . "just noticed I hit 50k")
    (("BrighterMalphon"
      . "\"the aztecs had the right idea, the sun *did* keep rrising, after all\"")
     "Ikatono" . "this is a fabius-style question")
    (("lambda_alpha_omega" . "@yellowberryHN y'all?") "Ikatono"
     . "@yellowberryHN \"one\"?")
    (("xivandroid" . "and novel...") "Ikatono"
     . "oh right, how detailed do you need it? Is \"debate bro\" enough?"))
  )

(defconst w/dna-cr4zyk1tty
  '((("[VOICE]"
      . "rather than colon opening parenthesis i played the original a while ago yeah i see i see i've heard survival horror is one of these things that like i'm very")
     "CR4ZYK1TTY"
     . "gomoco! cr4zyk3WaveAnim I made a bluesky bot last night, moved all of the posting code to my spare tablet. when I woke up it didn't work because I forgot to enable wakelock in termux. Then I slept again and now everything works overnight :)")
    (("[VOICE]"
      . "write CGI scripts in Perl 10-0, Air Homer 99. The, this is honestly not, if you know, if you know of the Pubnix Air Homer 99.")
     "CR4ZYK1TTY" . "!boosts")
    (("zoft_fae" . "lcolonAmor lcolonAmor") "CR4ZYK1TTY"
     . "!resolution be the first person to get to -100 tsoob")
    (("[VOICE]"
      . "It's been a hell of a time. It's been a hell of a time. I think I don't know I feel like I have not achieved my resolutions at all been here my whole life. Yeah same same")
     "CR4ZYK1TTY" . "I did my resolution :)")
    (("asrael_io" . "Let him simmer") "CR4ZYK1TTY" . "LetHimJoel")
    (("liquidcake1"
      . "Zip bombs can be detected prior to extraction at least.")
     "CR4ZYK1TTY" . "duck!")
    (("quasiconformal"
      . "(I'm just too timid... thanks for the streams, though!)")
     "CR4ZYK1TTY" . "Clap")
    (("[VOICE]"
      . "this is we'll judge your clone yes what if we made your clone like super boisterous and talkative thank you for the gifts of also liquid cake one this is this is too kind of you this is too")
     "CR4ZYK1TTY" . "speaking of clone :D")
    (("[VOICE]"
      . "This is fearsome. This is fearsome. You should ban whoever just did Idol's Dream. Oh my gosh. Hello, hello, quiet. Thank you for the 105 streak. Also quiet, Chibi Kaneko.")
     "CR4ZYK1TTY" . "I even have an idea for prompt :D")
    (("a_tension_span" . "*known") "CR4ZYK1TTY"
     . "every message must include a random number between 0 and 9999 and must rate it between 0 and 5 stars")
    (("[VOICE]"
      . "Ian slash slash tomato right this is this is important we have to learn about the whole history of the food that we're eating currently especially if it's something outside of your")
     "CR4ZYK1TTY" . "literally the shindigs thing xD")
    (("bigbookofbug" . "guix is amazing tho") "CR4ZYK1TTY"
     . "JumpingOnEveryLion")
    (("yellowberryHN" . "Joel") "CR4ZYK1TTY" . ":O")
    (("[VOICE]"
      . "go.el yeah eventually we do whenever our next elist project we'll just name that with no context i think uh the uh hello vincent underscore cs")
     "CR4ZYK1TTY" . "Joel")
    (("[VOICE]"
      . "huge memory. We could probably find it somewhere. SPCL does so as well. Allocates a lot of address space then maps the core file onto that space. Yeah, yeah.")
     "CR4ZYK1TTY" . "Joel")
    (("[VOICE]"
      . "I want to remake Bless again. I'm not satisfied with Bless where it is right now. I want to redo it again. I want to redo it again. I'm literally going to rewrite it again.")
     "CR4ZYK1TTY" . "xD")
    (("[VOICE]"
      . "on my computer with with comparative ease, right? We could do it in PureScript, but the thing is I want both. I would like to have both.")
     "CR4ZYK1TTY" . "bless 3 LETSGO :O")
    (("[VOICE]"
      . "How is this clone five or clone six 50k is a lot Yeah, LG LG has redeemed this like six times. I think spirit warrior five times")
     "CR4ZYK1TTY" . "clone +1 :O")
    (("RayMarch" . "its a whole thing") "CR4ZYK1TTY"
     . "@pralkarz gn cr4zyk3WaveAnim")
    (("[VOICE]"
      . "Cause none. Okay, exciting. Also, um... Why is this? Why the fuck is that happening?")
     "CR4ZYK1TTY" . "jol"))
  )

(defconst w/dna-colinahscopy_
  '((("[VOICE]"
      . "like oh you can't touch that you're not 21 she was like unless you are")
     "Colinahscopy_" . "ohi modclonk")
    (("[VOICE]"
      . "I'm not wise. I mean, I'm not the one saying that. It's them saying it. Only the youth take on bagging. I mean, I really don't think she was. She was at all.")
     "Colinahscopy_" . "Clonk putting foot in mouth again yarrLUL")
    (("s9tpepper_" . "was she looking for a sugar baby? LUL")
     "Colinahscopy_" . "oof IBS is no fun'")
    (("[VOICE]"
      . "Thank you Yes, it's okay sure you carry your ID to the grocery store because yes, you're gonna get carded yeah")
     "Colinahscopy_" . "bye maude yarrWave")
    (("a_tension_span"
      . "What if there is a link between the name Darius and the language Haskell? Maybe being named Darius pre-determines an interest in Haskell later in life? zkiClap")
     "Colinahscopy_" . "Cheer100")
    (("[VOICE]"
      . "I'm learning Haskell with a friend in the new year. This is extremely good. I hope I hope you have fun doing this We have this Okay There's there's Are you?")
     "Colinahscopy_" . "wait, it was 5 gallons of chili?!")
    (("Blaksun100" . "yep") "Colinahscopy_" . "good god")
    (("XorXavier" . "EvilJoel") "Colinahscopy_"
     . "i wish i had the drive to run, i don't find it that enjoyable")
    (("MeteoraVT"
      . "meteora did karaoke today. Also got our software approved on steam meteor68Corpa good day")
     "Colinahscopy_" . "the game is primeagenYapp and lcolonType")
    (("a_tension_span" . "Not even me, it was my clone") "Colinahscopy_"
     . "3k away from clone, lets go PogChamp")
    (("[VOICE]"
      . "those places that's where we go I thought of a new slogan to say wait this is this fuck fuck I thought of a new slogan to say the other day")
     "Colinahscopy_" . "oh?")
    (("NineteenNinetyX" . "USER STORIES") "Colinahscopy_"
     . "time to lockin")
    (("NineteenNinetyX" . "oh that Slack") "Colinahscopy_" . "Jon Bob?")
    (("[VOICE]"
      . "There's this thing there's this thing called slackware. Do you know about it slackware is a linux distribution? um It's a it's a linux distribution")
     "Colinahscopy_" . "oh yeah a flavor of linux")
    (("[VOICE]"
      . "Like, I'm not sure exactly what I'm going to do there yet, but I think this is the next step in this domain. But what is the third project? What is the...")
     "Colinahscopy_" . "like perl bless?")
    (("[VOICE]"
      . "I think I've seen you, we've encountered each other before. We've encountered each other by virtue of, I think spherophobia related into.")
     "Colinahscopy_" . "dope, back to back raids")
    (("[VOICE]"
      . "big hash I just have it set up to like replace the hash with this because I don't often have to see it the the")
     "Colinahscopy_" . "clonk out here leaking hashes")
    (("LCOLONQ" . "I use nix btw") "Colinahscopy_" . "ooh steam deck")
    (("GamerGirlandCo" . "i'm starting to feel sleepy ") "Colinahscopy_"
     . "emudeck lets go")
    (("[VOICE]"
      . "Imagine this, maybe you have a bunch of buttons, why yellow is green, I mean you change the highlight. The highlight is the inside.")
     "Colinahscopy_" . "clonkNET lcolonGlitch")))

(defconst w/dna-trap_exit
  '((("asrael_io" . "ubuntu") "trap_exit"
     . "There is substantially more \"open source\" software than \"free software\"")
    (("SlendiDev"
      . "i dont like how the term open source is used in marketing, you have stuff like vsc*de that requires you to agree to terms and conditions to contribute for example")
     "trap_exit"
     . "A lot of people don't realize that while GPL is more popular the BSD/MIT/Apache world has a lot more code")
    (("yellowberryHN" . "https://trisquel.info") "trap_exit"
     . "Zero money in FOSS didn't last long. By the late 90's we had the dot.com boom and Linux was front and center.")
    (("anothercommoninternetuser"
      . "I would just want to work on the doodles")
     "trap_exit"
     . "Early on Google was cool. They became successful for good reasons.")
    (("[VOICE]"
      . "Microsoft Google switch is a decline as a client side gizmo for canonical chat a doable thing or anti to us That's probably doable")
     "trap_exit"
     . "For those of us to remember when Google search engine dropped and how they just murdered all existing services.")
    (("nichePenguin" . "MAUDE") "trap_exit" . "downgraded")
    (("[VOICE]"
      . "Working on a rootkit multi-call binary builder. Sabotage looks interesting. Oh, hell yeah, very cool. Okay, yes, I have this interest for sure. I am interested in the problem of...")
     "trap_exit"
     . "MacOS's directory full of deps that are presented as a single executable and self contained images is perhaps the best compromise")
    (("asrael_io" . "base64?") "trap_exit"
     . "A number of scripting languages will package things in zip and then the runtime abstracts the fs calls to read from that zip")
    (("[VOICE]"
      . "words Andy over here, ASCII 85. Like, hmm, hmm, hmm, hmm, hmm, hmm, hmm, hmm, hmm. Like,")
     "trap_exit" . "Erlang however is removing that feature")
    (("[VOICE]"
      . "this segment is this range this section is this range etc etc right if you just have like a like and and I think")
     "trap_exit"
     . "C++23 is adding #embed to make embedding assets easier but will consume ram")
    (("[VOICE]"
      . "mooky sticks thank you for badass eating cereal uh funky linker stuff and like no load i'm wondering if you just like put bites at the end i guess how would we test this biblical")
     "trap_exit"
     . "@XorXavier I thought I had read it did. Maybe misremembering.")
    (("[VOICE]"
      . "My job is not on the line, I can just do whatever, so for me it's more pleasant.")
     "trap_exit"
     . "Like anything... a distro is a means to an end... what is your end?")
    (("asrael_io" . "the thread is very awake") "trap_exit"
     . "could do pause()")
    (("yellowberryHN"
      . "@bigbookofbug all of my computers are currently all different distros, but they will all be migrating to nixos in the near future given my recent experiences")
     "trap_exit" . "it would be in RES")
    (("[VOICE]"
      . "Right, which is kind of sick. Uh, like like, you know, there's there's cool shit you can do. Uh, the uh But like it's also like kind of")
     "trap_exit"
     . "Proton was developed by Valve so naturally their device uses it :)")
    (("ctrl_o" . "lcolonSoTrue") "trap_exit"
     . "Form opinion... or else :D")
    (("chromosundrift" . "we were so close to coding") "trap_exit"
     . "There are negative rights and positive rights.")
    (("CR4ZYK1TTY" . "cr4zyk3Joel") "trap_exit"
     . "@XorXavier unionfs on Linux is basically overlayfs before overlayfs")
    (("tayiorthegreat" . "enjoying the stream") "trap_exit"
     . "@XorXavier https://en.wikipedia.org/wiki/UnionFS there are a number of \"union filesystems\" around... including my own mergerfs. They all focus on somewhat different usecases though.")
    (("[VOICE]"
      . "next. And let's consider just foob this guy, right? Does this work for us?")
     "trap_exit"
     . "@XorXavier Yep... https://github.com/trapexit/mergerfs")))

(defconst w/dna-peercoin
  '((("crazykitty357"
      . "gg[g#]gfg[CD#cG#][D#][CG#f][Cd#][Cc]C[Cd#]/[DFfd][FA#][DA#f]D[Dg][A#f][Dd#a#]f[GBgd]B[Gd#][GDc][Gd#]G[Gd#]/[D#Gc]G[D#cg][D#g][D#g#][dg][D#f][d#d#][D#Ggc]f[D#][D#Gg][D#c][D#][D#c][d#][DFdA#]F[DA#d][Dd][Dg]/[Da#g]/[D#d#][D#][D#][D#][D#][FD#][GA#][fd#][gA#]")
     "peercoin" . "why are you spinning?")
    (("khlorghaal" . "okay so bad apple itself is... not? the VM")
     "peercoin"
     . "you wrote a c parser for fun, but it wasnt that much fun?")
    (("octorinski" . "hard agree") "peercoin"
     . "is your iq 200+ or something?")
    (("imgeiser" . "he didn't say hi to me, I'm leaving.") "peercoin"
     . "they should ship the hardware with the software right")
    (("leadengin" . "red") "peercoin" . "stuff++")
    (("imgeiser" . "hey clonk you clonker (this was sent from hexchat)")
     "peercoin" . "viking style")
    (("jallabee" . "addict pipeline") "peercoin"
     . "https://tmdc.scene.org/")
    (("asquared31415" . "OMG YES") "peercoin"
     . "https://www.youtube.com/watch?v=vcK4mF_RHDA")
    (("peercoin" . "https://www.youtube.com/watch?v=vcK4mF_RHDA")
     "peercoin" . "yes")
    (("imgeiser" . "is SICP difficult?") "peercoin"
     . "i like how they use letters, i mean, \"normal\" text mode demos use full squares in different shades and stuff")
    (("modclonk" . "lcolonHi") "peercoin" . "do you smoke?")
    (("asquared31415" . "spinne") "peercoin" . "o7")
    (("imgeiser" . "hey clonk") "peercoin"
     . "if you create a font where the characters are all the possible different combinations of pixels , do you consider that cheating?")
    (("zedzark"
      . "peercoin: depending on the size of the font, that could be anything from cheating to an impressive technical feat")
     "peercoin" . "@chromosundrift 4096 chars , idk ?")
    (("chromosundrift" . "for an 8x8 pixel font, that's 16k chars")
     "peercoin" . "oh right")
    (("asquared31415" . "i don't") "peercoin"
     . "@chromosundrift o_O would be impressive feat i guess")
    (("chromosundrift" . "haskellscript") "peercoin"
     . "@chromosundrift true, idk if a font can have that many chars tho, probably possible still with hacks like using multiple fonts and switching on the fly or something")
    (("imgeiser" . ">:(") "peercoin" . "or maybe not")
    (("prodzpod" . "isnt that just unifont") "peercoin"
     . "oh that is nice")
    (("prodzpod" . "oh its probably this") "peercoin"
     . "https://unifoundry.com/unifont/png/plane00/uni0010.png")))

(defconst w/dna-raymarch
  '((("fartingle" . "Woah there. Carnality") "raymarch"
     . "hi everyone nkoHi")
    (("ryasuar" . "like a mathmatical tapas") "raymarch"
     . "learning about math would be so cool, if it wasn't for the way mathematicians write their stuff down")
    (("liquidcake1"
      . "@running_out_of_unames Oh, hang on, what does \"their chance to be freed is gone\" mean. The game carries on for all, or the game ends for everyone?")
  "raymarch" . "i like it but i hate it but i like it")
    (("running_out_of_unames" . "Yes, prisoners can go more than once.")
  "raymarch" . "whats the picture at the bottom right btw")
 (("liquidcake1" . "PHP is so good.") "raymarch"
  . "something animal abuse or something")
 (("khlorghaal"
   . "gigabrain moment; define constexpr cex; define static constexpr sex; my codebase now has sex ass")
  "raymarch" . "they only know if its the other prisoner's birthday")
 (("tomaterr" . "27 is finite") "raymarch"
  . "are we in this prison right now?")
 (("danktownbunny"
   . "that's also assuming none of the prisoners lose their minds within the first year in solitary")
  "raymarch" . "am i the one who needs to turn the lights on?")))

(defconst w/dna-azrhyga
  '((("deep_field" . "I'll leave you to your pre-stream ritual")
     "azrhyga" . "Every day the starting screen is better and better")
    (("deep_field" . "Hello") "azrhyga" . "@MODCLONK Hi MODCLONK!!")
    (("celeste_kyra" . "Clonk! You haven't streamed in SO LONG")
     "azrhyga" . "Hi everyone!!")
    (("irregularmaterialist" . "jetsWave hello") "azrhyga" . "Awesome :0")
    (("fn_lumi" . "New Year's resolution") "azrhyga"
     . "Hi @CodeSpace0x25!!")
    (("tyumici" . "I thought you didnt like animals") "azrhyga"
     . "Bad Apple")
    (("kuromaruoniisan" . "https://www.youtube.com/watch?v=vV-5W7SFHDc")
     "azrhyga" . "!resolution")
    (("zulleyy3" . "React Native") "azrhyga"
     . "The best gift in the world ever created")
    (("stuxvt" . "nope nvm lmfao") "azrhyga"
     . "I have to go off the stream, bye LCOLONQ and Clonkers, have everyone a great day!!")
    (("deep_field" . "lcolonGGG") "azrhyga" . "Joel")
    (("azrhyga" . "Joel") "azrhyga" . "Also hi @Deep_field!!")
    (("deep_field" . "C lab 2021") "azrhyga"
     . "Today looks like is a stream about sea life")
    (("modclonk" . "@GenDude Hi!!") "azrhyga"
     . "@steeledshield It can be")
    (("azrhyga" . "@steeledshield It can be") "azrhyga"
     . "Also hi @MODCLONK!!")
    (("crazykitty357" . "jetsWave [i](this was sent from godot)[/i]")
     "azrhyga" . "C# food? Delicious")
    (("steeledshield" . "I am under the water. help me lcolonSadge")
     "azrhyga" . "Today is time to go the C underwater")
    (("tyumici" . "you will go saturation diving") "azrhyga"
     . "An 8ball command will be great to have here")
    (("destinywaits" . "w'all folks r gaming fr") "azrhyga"
     . "The next time surely we will see LCOLONQ channel in the Partner program, I hope to see it soon")
    (("tyumici" . "[First MI Last - Lat/Lon location]") "azrhyga"
     . "Josh Csharper sounds a good name for me")
    (("het_tanis" . "Dig Bickums") "azrhyga"
     . "Yes yes, I know it is randomly, but it was a suggestion of one name")))

(defconst w/dna-krzysckh
  '((("[FRIEND]"
      . "green dot again? it's like a confirmation of all the happy thoughts! let's keep the fun going! 🎈🌈")
     "krzysckh" . "hi computer")
    (("bigbookofbug" . "hello !") "krzysckh" . "hi big bookof bug hiii")
    (("[VOICE]"
      . "Perhaps for you as well, hello fn underscore lumi. For me basically, my intervening period. Hello steeled shield too, and hi sparkyluxray, welcome.")
     "krzysckh" . "!guy")
    (("Watchmakering"
      . "do you have a favorite horror genre? supernatural, cosmic, slasher.. ?")
     "krzysckh" . "the \"real life\" keeps me horrified enough")
    (("a_tension_span"
      . "@ctrl_o I feel challenged to do that now, why did you post that?")
     "krzysckh" . "like 2 of my websites are held by perl cgi scripts")
    (("zoft_fae"
      . "I have no knowledge thereof; I merely desire that thou might have a fair day. @fn_lumi")
     "krzysckh" . "i love perl")
    (("[VOICE]"
      . "stream VODs to YouTube and cry. I mean, we do do that, right? We do release a number of VODs. How does one even make a PubMix without any tomfoolery from users? Well, in...")
     "krzysckh" . "we are cool")
    (("benito_mescalini"
      . "“we do not create mischief” I see, I will see myself out")
     "krzysckh" . "i keep my database +rwx because \"we are cool\"")
    (("[FRIEND]"
      . "hey ctrl_o, how's that solar song collection coming along? only 0 out of 20 cards? let’s catch those cards! 🌞🎴✨")
     "krzysckh" . "https://grimgrains.com/site/home.html")
    (("benito_mescalini"
      . "@xorxavier what would a programmers luch be, an energy drink an sugar cookie?")
     "krzysckh" . "good recipes tm founded by the 100 rabbits people")
    (("[FRIEND]"
      . "hey liquidcake1, nice job on the copfish collection! 2 out of 111 fish so far? keep casting that line! 🎣🐟✨")
     "krzysckh" . "thank you control o")))

(defconst w/dna-katlyranna
  '((("MODCLONK" . "lcolonWiggly lcolonWiggly lcolonWiggly") "Katlyranna"
     . "hello @computer")
    (("yellowberryHN"
      . "Man I was just thinking \"wouldn't it be so funny if he came in right at the end of that\"")
     "Katlyranna" . "you changed your mic?")
    (("mickynoon" . "BETTER") "Katlyranna"
     . "it's not bad, it just sounds a little different than it did last time")
    (("a_tension_span" . "@MODCLONK SeemsGood") "Katlyranna" . "Joel")
    (("[VOICE]"
      . "I don't know how this even happens. How does this happen? Hello Retroboy128 the game dev. They're game- they're- they just say game on the top instead of game boy.")
     "Katlyranna" . "my GB operator arrives today katlyrAYAYA")
    (("Tomaterr" . "lol, lmao") "Katlyranna"
     . "wonder if that's insurance related after the foam pit")
    (("[VOICE]"
      . "I don't even know if this is, if this is real. Um, ball pits are a menace to society. Imagine dying. Hello, sweet robo. Congrats on first.")
     "Katlyranna" . "ball pits are fine, foam pits are not")
    (("Tomaterr" . "the west coast is the best coast") "Katlyranna"
     . "unless it's a furry convention")
    (("liquidcake1" . "It probably all devolves into sex0r.")
     "Katlyranna"
     . "I've been playing one since the late 90s but not many people play these days")
    (("fcollector"
      . "Iron Realms is the paytowin one, and one of their MUDs is/was literally the best game. Incredibly p2w though")
     "Katlyranna" . "necromium dot com")
    (("BRC_Del" . "you know what? I'm logging into OpenFusion")
     "Katlyranna"
     . "unfortunately everquest and wow hurt the muds a lot :(")
    (("[VOICE]"
      . "like maybe some ostensible mechanics um tomato clone is always at a spot soft soft for a chaos")
     "Katlyranna" . "necro used to peak 70-100")
    (("kiskiller0" . "what do you use for design, animation and whatsnot")
     "Katlyranna" . "I miss that")
    (("prodzpod" . "definitey") "Katlyranna" . "yep...")
    (("NineteenNinetyX" . "kismesis") "Katlyranna"
     . "I think muds could make a large comeback through discord")
    (("eudemoniac" . "lcolonType") "Katlyranna" . "LUL")
    (("rektdeckard" . "great. we did it guys") "Katlyranna" . "LUL 󠀀"))
  )

(defconst w/dna-the0x539
  '((("a_tension_span" . "The flashing has stopped but picture froze")
     "The0x539" . "hiya clonk")
    (("[VOICE]"
      . "you are stuck on something like FB term. Hello, Gerd Burger, also. Chaun doesn't do FB dev for images. Yes, yes. Okay, we're still seeing this flashing.")
     "The0x539"
     . "I have beef with kitty because I was personally subjected to the kitty/KiTTY confusion as a teen")
    (("[VOICE]"
      . "I don't know. I don't know. It's hard to we can discuss this unless yes, okay I'm gonna hide this in case it changes to another video device. That is a camera aimed directly at my head")
     "The0x539" . "Unles")
    (("Psychic_Refugee" . "Good day y'all lcolonLove") "The0x539"
     . "I argue it was slashing and hacking")
    (("[VOICE]"
      . "Drifted a little far off, of course. John entity, John object, John subject, John noun. Yeah, yeah.")
     "The0x539" . "'john is morphing into a pronoun")
    (("[VOICE]"
      . "If we wanted to compile C, for example, to brain, then this would be annoying. This would be royally annoying, actually. Probably compile to a GIF.")
     "The0x539"
     . "brainfuck can be implemented as a rust declarative macro")
    (("The0x539"
      . "brainfuck can be implemented as a rust declarative macro")
     "The0x539" . "I'll post on discord")
    (("SlendiDev" . "intel deprecated legacy boot") "The0x539"
     . "Posted the brainfuck rust macro on discord lcolonGreen")
    (("plurshie"
      . "they don't cater to weirdos who think about compilers 24/7 anymore")
     "The0x539"
     . "I like the business model where websites tell my browser to show hostile content and my browser does not")
    (("[VOICE]"
      . "is dry uh available don't tell me don't tell me i'm playing i'm replaying inquisition right now i hope you at least enjoyed it but please don't actually tell me uh i like to try to")
     "The0x539" . "gaming")
    (("XorXavier" . ":O") "The0x539" . "What is Colonq's skin color?")
    (("[VOICE]"
      . "The pinned tweet at Twitter.com slash OaklandQ has a reference sheet.")
     "The0x539" . "I do behold")
    (("XorXavier"
      . "i sort of have a pseudo filesystem where the generated header files contain names and groups for the static assets")
     "The0x539" . "include_fs crate")
    (("chromosundrift" . "classic") "The0x539" . "nasal demons")
    (("XorXavier" . "ok it happened im linking the talk") "The0x539"
     . "have you not heard of the nasal demons?")
    (("XorXavier" . "this is nasal demons talk") "The0x539"
     . "> In the C programming community, undefined behavior may be humorously referred to as \"nasal demons\", after a comp.std.c post that explained undefined behavior as allowing the compiler to do anything it chooses, even \"to make demons fly out of your nose\"")
    (("[VOICE]"
      . "to enjoy the Steam Deck, the UX is not straightforward. If you want to do non-standard things with it, I guess, but the baseline just runs Steam.")
     "The0x539" . "disagree")
    (("[VOICE]"
      . "in seems fine to me the desktop mode is pretty janky uh i find i find the baseline just just go steam style mode to be pretty")
     "The0x539" . "it \"just works\" if \"just works\" is what you want")
    (("chromosundrift" . "estate") "The0x539" . "seclusion?")
    (("wyndupboy" . "@pralkarz g'night") "The0x539"
     . "there are a bunch, I think winnow is the latest and greatest"))
  )

(defconst w/dna-fcollector
  '((("loweffortzzz" . "help") "fcollector" . "linguist")
    (("[VOICE]"
      . "There's a logo for Ashley, there's a logo for Cameraman, there's a logo for Sally Slices.")
     "fcollector" . "???")
    (("[VOICE]"
      . "copy and paste one that we already have somewhere. Ocaml.yell, where's our other language support? Is forth doing this?")
     "fcollector"
     . "I have  :mode (\"\\\\.ml[ily]?\\\\'\" . tuareg-mode)")
    (("[VOICE]"
      . "thing. We probably need to install the thing. Hell, he doesn't have Merlin around. What if I just say packages.merlin?")
     "fcollector" . "ocamllsp is more fashionable than merlin")
    (("GenDude" . "The lead grows") "fcollector"
     . "This is the second time I've ever seen anyone use objects/classes in Ocaml.")
    (("甲斐の虎武田晴信" . "O-O!") "fcollector" . "bye nugget")
    (("[VOICE]"
      . "has type unit, string unit, which is not a record type.")
     "fcollector" . "\"string\" ^ \"concat\"")
    (("[VOICE]"
      . "p mode but i'm not very happy with it uh begin and end do i need to like semicolon this shit")
     "fcollector" . "Semicolon, yeah")
    (("[VOICE]" . "library do? lib.hx here? What do you what do you do?")
     "fcollector" . "Does it not do the #error thing for Python?")
    (("[VOICE]"
      . "back-end kind of invokes like the generic evaluator back-end for like a certain part of the standard library and I just don't know where that lives. I can't find the...")
     "fcollector" . "Oh hmm.")
    (("ctrl_o" . "this guy brings the boom") "fcollector"
     . "I can kind of parse OCaml, mostly, but I can't keep up with this codebase at all at this speed")
    (("[VOICE]"
      . "I can kind of parse OCaml mostly, but I can't keep up with this codebase at all at this speed. Yeah, again, I'm just going, I'm going very fast, I apologize, because I'm just, like, I'm using")
     "fcollector" . "Check defines is just unit, no exceptions")
    (("[VOICE]"
      . "to is it is it curry like what's the what's the deal here is it like this oh camel hello 715")
     "fcollector" . "(fun a b c -> ...)")
    (("[VOICE]"
      . "Um, like, a project called Driving, yeah, um, Haskell, you have to implement your own printing routine.")
     "fcollector" . "That's going to require some kind of ppx tomfoolery")
    (("agpen" . "not dead") "fcollector" . "Dies for a second, it's fine")
    (("[VOICE]"
      . "I guess the thing that loads the classes is the thing to look at, right? Presumably they're going to be called with the same...")
     "fcollector"
     . "What's the path type? Presumably there's a string in there somewhere")
    (("[VOICE]" . "SFX does it can we can we see can we see")
     "fcollector" . "oh gof")
    (("[VOICE]"
      . "XML checks this thing, like where is it being made though? Where are we adding to see?")
     "fcollector" . "That's the musk of universal harmony")
    (("[VOICE]"
      . "goes the e-lisp it's um there's no e-lisp yet uh there's not even")
     "fcollector" . "yet to go")
    (("[VOICE]"
      . "They can be resurrected. Yeah, I feel this, I feel this way strongly. It's, it's always possible to like, you know.")
     "fcollector"
     . "And then there's the impulse decisions to just whip up a transpiler between two weird languages using a mess of an established codebase in a third foreign language..."))
  )

(defconst w/dna-diabloproject
  '((("Azrhyga" . "Hi @semaphriend!!") "diabloproject"
     . "Hello clonk & computer")
    (("SparkyLuxray" . "the electric comerce risaPog") "diabloproject"
     . "Oh, black friday")
    (("Azrhyga" . "Also hi @brymoi!!") "diabloproject" . "Why projectT-T")
    (("octorinski" . "Joel") "diabloproject" . "No, i mean projEct")
    (("[VOICE]"
      . "I think I think it might be time for it might be time for that It might be time for that a little bit. It might be time for let's let's see. Let's see buttons work buttons should work")
     "diabloproject" . "@LCOLONQ black Friday discounts for equity?")
    (("PeetsEater" . "Buy one get one free") "diabloproject"
     . "You can hire a guy for it XD")
    (("Katlyranna" . "KEKW") "diabloproject"
     . "krzysckh is cloning, innit?")
    (("[VOICE]"
      . "katli rihanna also um this is this is this is also true katli wait why why do i say rihanna it's it's katli rana")
     "diabloproject" . "how to get 40k channel points in one stream")
    (("[VOICE]"
      . "I just say Bible canonized user and that's and that's on compiling J isn't it that's on compiling J.")
     "diabloproject" . "No lord for me Sadge")
    (("[VOICE]"
      . "Welcome in Cocoopsie, it's great to have you. You're in a stupor? Is this, what is the meaning of the stupor? What has led to the stupor?")
     "diabloproject" . "How many subs for discounted equity lord lol?")
    (("[VOICE]"
      . "complex like derivatives market here, I think, right? This is what I think we need to go. Eudaimoniac was suggesting this a while ago.")
     "diabloproject" . "LCOLONQ Stock trading exchange")
    (("emailschaden" . "clonkbucks (canadian)") "diabloproject"
     . "@paxonian about 30 secs of conversation")
    (("[VOICE]"
      . "I think just more so, I feel like, I don't know, I think it's hard to not be like quote unquote productive.")
     "diabloproject"
     . "@LCOLONQ Where can I find your gba assembly? I feel like doing stupid things today")
    (("[VOICE]"
      . "see in books a lot and read about this why was this a specific approach taken yeah what are the trade-offs yeah not not obvious ones yeah yeah this and like sometimes there's not even a good place to put a")
     "diabloproject"
     . "LLMs are kinda useful, but I more interested in LLMs themselves, not in the result. I like them for what they are, not for their usefulness")
    (("[VOICE]"
      . "I really find I want like the... I want something that is very English, very human. Thank you Taurus for the follow too. If you're new here, if you're just arriving and I haven't weirded you out with...")
     "diabloproject"
     . "Attentions, contexts dataflow and other stuff in models are really cool. Especially if you will try to visualize or imagine it")
    (("[FRIEND]"
      . "whoa, LCOLONQ just snagged the controversial knife from shindig's shindaggers knife collection! that’s awesome! 🔪✨💥")
     "diabloproject"
     . "Yeah, that what's chatgpt and other bourique models lack")
    (("[VOICE]"
      . "it recently but i haven't i haven't played with it very much at all the uh the like i don't know i don't i don't know like i think i think like the things i would")
     "diabloproject" . "You cannot teach them")
    (("[FRIEND]"
      . "yay, LCOLONQ has caught 5 fish in the copfish fish catching collection! keep it up! 🎣🐟💖")
     "diabloproject" . "Oh")
    (("stoicmana" . "everything always breaks on cloud apis")
     "diabloproject" . "Realtime LLMs Aware")
    (("[VOICE]"
      . "potential in this in this area and I would I would honestly be shocked if people aren't doing it currently but I think I think there's probably a world where like the way the way I would do it is")
     "diabloproject" . "text adventure games are soo cool"))
  )

(defconst w/dna-ricardo_stryki
  '((("brandon_52s"
      . "Because on twitch when I live stream, there’s a mike symbol, and it’s crossed out")
     "ricardo_stryki" . "I love how ASCII VTuber, looks nice.")
    (("ellg" . "hail raiders") "ricardo_stryki"
     . "How are you putting videos on your hair? Is it done in the VRM?")
    (("fannyslam" . "i love this") "ricardo_stryki" . "very interesting")
    (("fatdirtymole" . "yeah") "ricardo_stryki"
     . "I always wanted to use CUDA when working with AI related stuff, but I prefer AMD for Linux, at least now we have ZLUDA.")
    (("abipolarcarp123"
      . "It's all linear algebra, right? That shit goes crazy")
     "ricardo_stryki"
     . "idk but for running models or training them they always use CUDA or CPU, I've never seen an AMD GPU option.")
    (("tomaterr" . "that shit went so hard") "ricardo_stryki"
     . "23 days ago, lmao")
    (("biggaymikey"
      . "I really dont understand the difference between Vulkan and OpenGL with something like DirectX")
     "ricardo_stryki" . "there is an \"Open\" button, it is not an image")
    (("fatdirtymole" . "LUL") "ricardo_stryki"
     . "OpenXR is important to VR too, and they made it")
    (("octorinski" . "Joel2") "ricardo_stryki"
     . "In my opinion, fuck propietary SKDs, walled garden for no reason.")
    (("fannyslam" . "oh we stackin") "ricardo_stryki" . "SDKs*")
    (("fannyslam" . "gUYS") "ricardo_stryki" . "what")
    (("setolyx" . "LOLLLL") "ricardo_stryki" . "opensource cum")
    (("notmelpherion" . "from cum to jerk, what has this stream become?")
     "ricardo_stryki" . "Is that org-mode with 1000 addons?")
    (("tomaterr" . "Bezelea are you not brothered out dude")
     "ricardo_stryki" . "step-bro")
    (("tomaterr" . "it's the Ghost in the Shell Finger GIf")
     "ricardo_stryki" . "how can you have 394 bpm and be alive")
    (("notmelpherion" . "of course it's WoW lol") "ricardo_stryki"
     . "is that BPM counter wrong? right?")
    (("notmelpherion" . "oh christ") "ricardo_stryki" . "ah ok")
    (("silicone_milk" . "show me the org-roam graph") "ricardo_stryki"
     . "Void Stranger Wattpad Edition")
    (("chromosundrift" . "you got it bad") "ricardo_stryki"
     . "light novel")
    (("chromosundrift" . "you could probably memory hack it")
     "ricardo_stryki"
     . "unrelated question, but can you sync org-mode notes to other PCs?")))

(defconst w/dna-retromaximusplays
  '((("YukieVT" . "peggyxComfy") "retromaximusplays"
     . "lcolonCool lcolonCool lcolonCool")
    (("[VOICE]"
      . "What are you- I'm not even going to try and make a bad dad joke about doors, haha, what about doors, is this uh, how does an app just lose input focus? Oh you're-")
     "retromaximusplays" . "lol")
    (("LeadenGin"
      . "I've got a little time left to enjoy the leaves, then I want the cold to arrive")
     "retromaximusplays" . "the doors. :)")
    (("sladoid" . "I'm not tired you're tired hahaha")
     "retromaximusplays" . "ceilings?")
    (("[VOICE]"
      . "like I don't know I I'm into that a little bit there was an arc in my life where I was I wanted to do more things with my hands computer I had this I had this")
     "retromaximusplays"
     . "thats cool. then maybe make the seed sharable. or maybe that might be  a little overscope for the jam.")
    (("XorXavier" . "o/ mod colonq") "retromaximusplays"
     . "that sounds about right. then once a confirmed path has been place you mght be able to gen random dead ends etc if needed.")
    (("[VOICE]"
      . "I'm not not not that I'm averse to staying to not being on task. I think I don't know I'm just I don't I don't want to I don't want to be an asshole about it. I this is")
     "retromaximusplays" . "dang i missed it what did the noodle say?")
    (("XorXavier" . "Joel") "retromaximusplays"
     . "ya not important. was curious about the timeout")
    (("chromosundrift" . "like cookie dough") "retromaximusplays"
     . "I made it. 50k channles points!!! hmmm now my clone plan begins. :)")
    (("car_batteries_into_ocean" . "mine is feeding the eels")
     "retromaximusplays" . "lol")
    (("[VOICE]"
      . "style and then I get good rest right you understand and now now today I'm like I feel like I'm in a state of it's not like a bad state but I feel like I'm in a state of")
     "retromaximusplays" . "zooter implies you partake in the greens")
    (("retromaximusplays" . "zooter implies you partake in the greens")
     "retromaximusplays" . "zooted*")
    (("LuckyBearThing"
      . "how to play any multiplayer game these days Kappa")
     "retromaximusplays"
     . "Mobile Legends and Kings of honor are great moba's should try. do recommend")
    (("[VOICE]"
      . "get like enthralled into. How to play any multiplayer game these days? Yeah. Mobile Legends and Kings of Honor. I know Honor of Kings this is like the big that was like the biggest game in the world that's the")
     "retromaximusplays" . "yup")
    (("[FRIEND]"
      . "oh boy, chromosundrift is still reeling in those fish, now with 44 out of 111 in the copfish fish catching collection! keep the nets cast wide! 🎣🐟🌊✨")
     "retromaximusplays" . "@chromosundrift  player verse environtment")
    (("XorXavier" . "this seems really cool") "retromaximusplays"
     . "omg little big plaent. havent heard that in years. the first Fall guys. sigh")
    (("CrazyKitty357"
      . "jetsNostalgia (this message was sent from ChatGPT)")
     "retromaximusplays" . "lil bit.")
    (("ellg" . "Cani arcanine nin canin..") "retromaximusplays"
     . "thats a good content fallback imo")))

(defconst w/dna-essento
  '((("[VOICE]"
      . "at the end here, right, is sub, I think we're calling it s to set flags, sub s.")
     "Essento" . "Good evening Computer and Clonq lcolonHi")
    (("[VOICE]"
      . "They would say I was an evil guy and throw me in jail. I would, I would if I could, you know. You already know, Brett or Malfoy. You're aware of my particular predilections.")
     "Essento"
     . "It took the essence of all good MMOs and turned it into a game")
    (("[VOICE]"
      . "If we try and mod web phishing, the modding thing is always the question, isn't it, right? It's always the ones where I'm like, it seems like it's a little bit suspicious if it's gonna happen or not.")
     "Essento" . "$$$")
    (("semaphriend" . "like any game that has fishing in it?") "Essento"
     . "Sounds good")
    (("steeledshield" . "worm your way in! 🐍") "Essento"
     . "Well, he did work on it at some point")
    (("[VOICE]"
      . "Not the missile. Not the missile. It's an indie MMO like you look at this site this site is actually awful because it makes it look like it's like a mobile game that's pay to win.")
     "Essento"
     . "The MMO knows where it is because it knows where it isn't")
    (("[VOICE]"
      . "strange it's it's very uh bizarre hazmat was playing this for a little while i think uh but like it's it's it's just it's just a really")
     "Essento" . "I really wanna check this one out at some point")
    (("[FRIEND]"
      . "another light blue dot! it’s like a gentle wave, let’s ride it and enjoy the quirky surprise! 🌊💖🎉")
     "Essento" . "It certainly seems like an experience")
    (("[VOICE]"
      . "soon as you like allow for patching right as soon as things are getting patched and tweaked and like improved right it just like like oh he loves shitty games")
     "Essento"
     . "I'm still to broke from getting a promotion to equity lord lcolonSadge")
    (("[VOICE]"
      . "oh are we not calling it in main loop? I thought we were. Oh we're still getting wrong we didn't fix that issue we encountered that issue and then did not fix it")
     "Essento"
     . "But now I'm getting less capital per stream since my time zone switched last sunday. Now I get 1 less hour of stream per stream lcolonMadge")
    (("[VOICE]"
      . "what if we toggle debug your timezone switch last Sunday now get one less hour of stream for stream okay a cento a cento my timezone")
     "Essento" . "Hell yeah")
    (("[VOICE]"
      . "collector also you just broke 50k too god damn god damn this is uh it must have been a fortuitous time whenever whenever everyone arrived it's it's")
     "Essento"
     . "We Europeans at least get some after-dinner hours of Clonq")
    (("[VOICE]"
      . "that's I appreciate that thank you I do feel welcome uh yeah it's it's kind of a fool's errand to try and like I think chase after a particular like like to try")
     "Essento"
     . "Or Clonq uploads himself to the computer and streams for 16 hours at a time")
    (("[VOICE]"
      . "um hello young zaniel i'd rather eat this t-shirt like like i feel like you could do a bunch of funny like there was a whole arc with that guy i know i know an")
     "Essento" . "Joel")
    (("Faeliore" . "Prod callout happened") "Essento" . "lcolonGreen")
    (("[FRIEND]"
      . "hey faeliore, how's that friendship blossoming with me? i’m all ears and lots of love! 🌼💖✨")
     "Essento" . "Good point, I should lcolonSnooze. Gonico lcolonHi")))

(defconst w/dna-leadengin
  '((("[VOICE]"
      . "the melody it didn't go is this is this the person it's that person there the okay so what is going")
     "LeadenGin" . "I have no BOOST and I must TSOOB lcolonHi")
    (("bigbookofbug"
      . "\"im not going to make a bad dad joke\" proceeds to make bad dad joke")
     "LeadenGin"
     . "I've got a little time left to enjoy the leaves, then I want the cold to arrive")
    (("XorXavier" . "@asquared31415 D:") "LeadenGin" . "hunter2")
    (("[VOICE]"
      . "else on your t-shirt yeah this could this is possible this is possible um the uh damn damn the optical optical id is sure completely covered with the")
     "LeadenGin" . "you should render yourself in letters IRL")
    (("anothercommoninternetuser"
      . "uses your cap to doetect your head rotation")
     "LeadenGin" . "a shirt with \"friend\" on it")
    (("[VOICE]"
      . "Model on shirt also with different hair on back. Oh my god. Jesus Christ the Okay, okay, but like")
     "LeadenGin" . "+2")
    (("[VOICE]"
      . "I made like a three page book one time, yeah. What I did a lot of the time is it's pretty easy to re-bind books, right? Like if you have a soft cover copy of a book...")
     "LeadenGin" . "compile-time game randomization? cool")
    (("woozle_ch" . "do we have a battle pass?") "LeadenGin" . "cave :>")
    (("Tyumici"
      . "Sounds like the perfect recipe for a nostalgic coffee-fueled gaming session!")
     "LeadenGin"
     . "Microtransactions verified through the Nintendo e-Reader")
    (("[VOICE]"
      . "They loaded that shit into the vbox And then below that again, this one is a different typeface. This one's a this one looks like it's a it's a sans serif font")
     "LeadenGin"
     . "text glowing, selections highlighted, gameplay flourishing")
    (("[VOICE]"
      . "They're all like this. They're all like this in a big way. Yeah. Yeah. And like the weapons screen, like.")
     "LeadenGin" . "I encountered a 2x5 font. it is very computer")
    (("neunmalelf" . "Thre is someting in \"unpolished\" or \"wring")
     "LeadenGin" . "idk what to call it, but yes, the subpixel one")
    (("JivanYatra" . "La Croix") "LeadenGin" . "Joel")
    (("[VOICE]" . "") "LeadenGin" . "Joeler")
    (("[VOICE]"
      . "as we hope things are hope things are treating you kindly. Where do these interstitial graphics come from? Well, that one I think they're linked down below. That one")
     "LeadenGin" . "Jol")
    (("[VOICE]"
      . "will. You feel like you're in the world of Pokemon. Okay, let's make it a little bit quieter. Hello, my Mimi underscore underscore underscore welcome in the composes")
     "LeadenGin"
     . "I thought the pokemon sounds were a halloween ghost bit until I figured out what they were")
    (("ellg" . "Ni canin, ni arcanine ni can") "LeadenGin"
     . "I was consistently surprised by long weekends because I forget when most holidays are")))

;; put DNA in chemical grade tube
(defun w/dna-put-in-chemical-grade-tube (dna)
  "Take DNA and put it into a chemical grade tube."
  (--map (cons (cdar it) (cddr it)) dna))

;; hydrochloric acid
(defun w/dna-apply-hydrochloric-acid (tube)
  "Pour hydrochloric acid into TUBE."
  (cons
   (-map #'car tube)
   (-map #'cdr tube)))

;; G5 50 solution
(defun w/dna-g5-50-solution (acidtube keyphrase prompt k)
  "Mix ACIDTUBE with G5 50 solution.
KEYPHRASE is incorporated into the mix.
PROMPT defines the clone's personality.
Send the result to lab station K."
  (w/ai
   keyphrase
   k
   prompt
   (car acidtube)
   (cdr acidtube)
   ))

;; self fusion machine
(defun w/dna-to-fake-chatter-profile (dna username color prompt)
  "Clone from DNA with USERNAME and COLOR and PROMPT in the self fusion machine."
  (let ((acidtube (w/dna-apply-hydrochloric-acid (w/dna-put-in-chemical-grade-tube dna))))
    (w/make-fake-chatter-profile
     :username username
     :color color
     :compute-likeliness #'w/fake-chatter-standard-likeliness
     :send-message
     (lambda (st)
       (w/dna-g5-50-solution
        acidtube
        (w/fake-chat-prompt-build st)
        prompt
        (lambda (msg)
          (w/fake-chatter-send st msg)))))))

;; NASA grade supercritical furnaces

(provide 'wasp-dna)
;;; wasp-dna.el ends here
