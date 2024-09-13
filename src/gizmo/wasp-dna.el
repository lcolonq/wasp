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

(defun w/dna-generate-from-logs (user)
  "Generate DNA from historical logs for USER.
You probably want to use this interactively and then save the result here."
  (let* ((logstr (w/slurp "~/logs/log-2024-07-23.txt"))
         (log (--map (cons (cadr it) (caddr it)) (--map (s-split "\t" it) (s-lines logstr)))))
    (-non-nil
     (--map-indexed
      (when (s-equals? (car it) (s-downcase user))
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
