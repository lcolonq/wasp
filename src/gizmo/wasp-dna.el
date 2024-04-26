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
  (let* ((logstr (w/slurp "~/logs/lcolonq-2024Q1.log"))
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
