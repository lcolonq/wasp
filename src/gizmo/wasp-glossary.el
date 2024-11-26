;;; wasp-glossary --- Virtual Prodzpod -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 's)
(require 'f)
(require 'wasp-ai)

(defvar w/glossary-inputs nil)

(defconst
  w/glossary-examples
  '("The good old days. Old model, incredibly timid clonk, actual programming instead of yapping for 4 hours. The legend starts here. Well, technically there is a Season 0 in a form of two(?) test streams he did before, but those are not left in the VODs, and only passed through oral legends."
    "The first like 8 streams started as a Pokemon Emerald GDB debug stream, writing a bot and discovering fun assembly stuff. His first word on stream,  PEMIS , and the pokemon cry at 22:07,  Fugiiiih! , are notable statements we still use occasionally. He planned to do a emerald gdb stream for his anniversary, but he didn't and made yuri (Girl's Love) instead (will be elaborated when we get there LOL)."
    "Oubliette of General  is a mystery dungeon-type fan game made for  General GEEGA . this is STILL in development. its been a YEAR. reminds me of a  certain game ..."
    "Like the name suggests, this is a big one. Beginning of FIG, The start of the modern clonkhead infrastructure. simple times where you could just blast amogus."
    "The end of the classic era (part 1), we start to see currently noted clonkheads. a lot of the redeems we see nowadays gets thought of here (basic palette swap, devipper...), the framework of the modern LCOLONQ Broadcast starts to settle in."
    "What the fuck. seeing \"LC\" still feels weird. he is LLLL. anyways, the second part of the finale. he actually just reads poems."
    "There was a \"lab session\" before this but every video from now on gets labelled lab sessions, until the actual Season 1 finale. Anyways, new start screen, real gizmo style broadcast beginning to show (although they are usually split into multiple streams still), this is a transition period from a Season 1 Pokemon Debugger to a Non-Canon OVA Oomfie Lord type streamer."
    "The introduction of BOOSTs. the first user stat to ever exist."
    "This is where we get the RPG system, the first of user stats where you can roll up a full AD&D character and level them up. also seemingly this is the first stream with the new starting screen."
    "This is the first \"pure shinanigans\" type stream not counting the wild times that was Poem Reading. Clonk filmed this outside, meaining there was a lot of wacky stuff going on. Also, this is where he works on rowobots, a uwu_to_owo robot tournament type thing that sits in his github. if you saw his github and always wondered what it actually does, this is the video."
    "THE episode. you know this one already."
    "The AI revolution is here. origins of fig-ask, fake chatters, forsen, monster kill..."
    "OBS has been connected to Emacs, a lot of the modern OBS based effects happen now (modclonk head, arrow, LCOLONQ live reaction)"
    "Despite its finale status, not much really happens outside the new model. but two crucial things happen. first, new palette swap meta. second, Mr. Green. yes. the Green Face of our Broadcast appears first in this episode."
    "The Longest period of LCOLONQ history so far, the Non-Canon OVA has begun. this is your usual modern clonk now. except we don't have \"friend\". this is messed up times actually. anyways, a TON of wacky shinanigans happen in this period."
    "LCOLONQ does shit in Gentoo and not NixOS. he will come a long way from this to TempleOS (maybe)."
    "Collab stream with JakeCreatesStuff, having two characters in the screen is kinda wild. this is NOT Bells of Bezelea, as that one is made by Bezelea."
    "Video Hair originates here. hair meta changed wildly since then."
    "Clonk GAMES (!). baldurs gate 2 on the ol gentoo. he barely games actually. real gaming is later."
    "JOEL MENTION. 7tv emotes are added and this means the beginning of the Joeling Era."
    "LCOLONQ reaches 1024 followers, and the first marathon stream begins. joel actually appears here."
    "Finally, \"friend\". and this opens a new era."
    "We are mostly on one bit per video session now. rapidfire rounds of feature additions will now begin. to keep this webpage loadable, I'll only embed the legendary clips."
    "This is also what is referred to as a  geneaology era , where clonk tried to grind for Twitch Purple and ended up making various one-off gizmos. kind of a low point but filled with good bits nonetheless."
    "Chatter Fusions are created here. Fusions are a currently disabled bit where two chatters are fused into a single chat message, with a GPT-generated name and message. ALSO this pubnix is created now."
    "Rooms are created here. Rooms are a chatter stat where each chatter is given a room to decorate with emotes. they are accessible via  https://colonq.computer/room/[number].gif ."
    "FORTH is mentioned here, and the crazy tale that sprawls into wackiness that is BLESS begins. gforth sucks btw."
    "Slots are made, theres a few gambling related remnants on the fig right now but most of this is disabled."
    "CURSE is made, which is a Elisp to Javascript transpiler. this is truly cursed."
    "Bells of Bezelea is introduced. we don't have chords yet, no multiple channels, no super idol good, just pure bells."
    "THE homestuck video. a must-watch for all clonkheads. The history of homestuck and andrew hussie is deeply alluring."
    "Sam Altman appearance happens here. this is the last recorded stream crash and double boost type situation."
    "No VOD of this remains, and it happened in the discord. everyone was in the call doing soundboards and voice calling, it was one weird ass stream. we did gforth, and gave up. gforth sucks."
    "AOC  leaderboard is set up. this is expected to return next year."
    "Ancestry, another chatter stat is established here."
    "Mountain Dew RAID event  was happening around this time. we go super desperate style its kinda funny. spoiler: mtndew dont come. dont expect it."
    "Various things happen here. Cloning, yearly recap..."
    "Clonk tackles css and fucking dies. him agonizing for a few hours isnt as fun as it sounds though."
    "Electron is made. this is not the real electron but rather some kind of native application development tool for emacs. it's cool. this will come up later."
    "Virgil is created. this is a bad apple based OS where each combination of 3x3 pixel determines a specific operation. He plans to run doom in it."
    "!resolution is created in celebration of the new year. this is yet another chatter stats."
    "\"real name\"/identity is added to the chatter stats."
    "\"friend\" is given an animal crossing-like voice system.  it still sounds like its farting."
    "api.colonq.computer is created, now everyone can fetch the various user stats. geiser.xpi is made in response. please check the  geiser.xpi page  for a summary of the chatter stats. oh yeah, also this is the finale of the Non-Canon OVA. we're going into the Canon Zone now."
    "We're officially back into the canon. with This Thing. very serious season. this new model is eventually going to be used to censor screens.  (it's a default live2d model or something.)"
    "Previously in L:Q  happens. this may return with various different voices (doubtful)."
    "clonk gains a check mark. I think clown motel became a bit but idk"
    "Second marathon stream. We play Dark Souls 1 with MODCLONK, it's absolutely worth watching its a wild time, we defeat the videogame, and then We create arguably the first \"meta-bit\", utilizing Electron to create \"Uhhh, What? The Relationship Between The Polarities In My Ikaruga Clone Are Surprisingly \"Yuri\"?!\" within the 20 hours. it's a parallel thinking style unintended fangame of  ikaruga . We also started a game jam, details in  the gamejam page ."
    "This season is a bit of a transitional period where clonk achieves post-purple nirvana and finally aims to tie up the loose ends of the various bits, and then suddenly THE INCIDENT happens and we end up with a completely new version of fig. we are heavily rebirth themed in this season. the amount of yap also increases as his viewerbase doubles once again from 3 to 4 hours. disruption is another major element of this season."
    "Newspaper is created. you can use the redeems to create an issue of the newspaper that is printed to the discord every stream."
    "!fish became actual pokemon emerald style. back then it just returned miss quote statically."
    "Geiser.xpi is extended into a clonkhead recognition program, where clonkheads can click each other in other people's streams and their BOOST scores will go up by 1 (away from 0, so negatives will get -1 BOOST). this is the first shakeup in BOOST economics since its inception."
    "We started working on two things, one is a API extension that will eventually completely replace the twitch channel point and redeem system, and the other is a meta-bit of \"Twitch Janitor Competition\" where various twitch mods are invited to moderate a fake chatroom with increasingly sillier rules."
    "Clonk comes up with the idea of Twitch Two: a moderation competition of exceedingly silly capacity. we'll see when this one happens."
    "Clonk GAMES yet another time, making a  balatro  mod."
    "There are also stream ideas that float around that isnt complete yet. This is not a comprehensive list."
    "clonk finally gets crushed by all the bits going on at once and crumbles. clonk actually dies and revives on the spot jesus style. clonk easter real. you wont know this actually happened until the vod is out lol"
    "the truth is out now ...."
    "we think of 99(+a) items for oub mode. oub mode becomes closer to completion. balls mode activate."
    "the lain watchalong happened, we all laughed incredibly awkwardly and lived present day, present times. we are truly the wired in irl life. geiser missed the watchalong despite being the one asking for it. f to geiser."
    "THE RECKONING - everything breaks and dies, burns up and disintegrates. we \"migrate\" our chatter stats to the redis mode, and its all messed up. which is crazy because this was before redis blew up by themselves. lol. no more redeems, no more factions, no more stats, no more emotes, no more chat even. well a bit worse than no more stats because stats got actively corrupted with emote mode. the most down we've been in years really, just in time for the shareholder meeting."
    "an extremely belated PAX East hotel room stream happened, where clonk employed funny mic and tried to measure the distance but got derailed anyways. the stream lasted 2 hours."
    "a whole 9 hour stream that was purely focused on bringing shit back. the chat, the emotes, the boost, the model, and finally, the friend. friend actually dies and becomes a ghost canon style. fig is replaced by wasp from here on out, although fig-server prevails."
    "we rebalance all the prices, FINALLY ADD ROSA MODE, and redeems in general change and shift tectonic."
    "we spent like 30 minutes doing shareholder meeting and then became immensely distracted with the vcvrack. we found out that every clonkhead is actually eurorack sisters and clonk vst making stream might be coming."
    "it's not an april fools stream. we actually work on oub (holy fuck progress). graphics are changed wildly so that the walls are less pixelated and more imagalated."
    "another iron in the stack: bless gets a strange vocal upgrade where each syllable corresponds to a function?? so that it can be programmed by simply speaking to it. idk where this goes."
    "we also introduced TOTAL CLARITY - the gong bit goes so hard. antipiracy was also introduced here."
    "In preparation for OFFKAI this year (they are going to offkai btw), he is LOCKING IN on the couple of his choice projects, notably oub mode and z80 game boy mode. this will most likely last until then. the yap style of the previous sub-season is lowered in favor of progress (kind of). will he finish the videogames until then? we shall see."
    "the gameboy z80 forth project revives, and this time its actually getting worked on. he works on the cpu mode and makes it run until vblank mode."
    "we also create the \"talk to clone\" redeem, where you can talk to clones like you do with \"friend\" or computer."
    "The z80 saga continues as he also recovers the BPM mode and displays... something? on the screen. something red, like edge, i guess. he does graphics, basically. this stream also adds the Joel/ICANT/+2/-2 counter in place of the faction status, making our stream 1 step closer to the northernlion dimension."
    "this is probably so inadequate explanation but im genuinely like this is haskell bros please help me lmfao 😹"
    "we go oub mode today as he showcases various fixations he has at the moment such as mudkip pmd faces, monads, dungeon crawl stone soup uwu copypasta and the graph. speaking of the graph, special guest me shows up on stream and talks about how ive seen ellg vtuber model in a korean yaoi web manhwa. this is also around where various prod v memes start spilling into the clonk heads and clonk head (inside), or something of this nature."
    "the  bingo  starts becoming a thing but i tuned it a little too much and we almost went for a blackout. the irc zone gets an upgrade where it is connected to various other streamer discord channels of the cyberspace such as  #geiserzone ,  #jakerealm  and  #prodarea . this is also modclonk (the peoples man) and us gamers him to an insane degree where he kills gamer INSTANTLY. gamer rebirths as a 5 charge sacrifice build later into the stream but we went to sleep 40 minutes after the stream with modclonk so we dont know what happened. Voice-activated commands are added such as \"LUA\" flashing a brazillian flag on screen, or ICANT increasing the ICANT counter."
    "we along with The People's Man pressure clonk into confirming GAMING SUNDAY mode. it will finally happen this week. we also tune the AIs a little bit down and increase the friend mode i think. mental clarity also returns i think ??? we also try another gameboy test thing and ends up with unintelligible output. we are closer to being done than start i think, halfway mode. sorry guys this is haskell again"
    "we talk about skibidi backrooms and go oubstyle, where he spends like 4 hours explaining how the oub code works and then finds out the Fake KVM mode but for keyboards (barrier mode) where key repeats are emulated by repeatedly releasing and pushing the button, causing disruption. we add HP style to player and enemy, and ability to kill, as well as some AI for the enemy. this is kinda gameplay i think"
    "the third ever After Dark session - and the first DISCORD ONLY EVENT (😱) we play stellaris for 13 hours. 13 people sign up and like a few people stuck around til the end i think. well 1 person is actually 2 people because noted clonkhead tomaterr and modToma are playing as one entity (the EvilJoel Empire). despite clonk's deeprooted paranoia and social anxiety everyone have a great time and surprisingly nothing really goes wrong. i mean youd think 13 people multiplayer would either end up in setting shit up hell or some kind of argument breaking out but yeah none of that. here are the list of participants and its empire, along with notable events of that nation. the game ended with Don't Worry About It and the Federation (mostly modclonk) clashing. Don't Worry About It wins the battle but not enough to completely demolish the nation and it ends in a modclonk victory i think"
    "the final map is thus." "the saga continues... (cpu is \"done\"?)"
    "clonk gets enraptured in a fey mood all of the sudden and introduces political debate into the stream (????), where a random chatter is selected to defend a random position on a random topic against \"friend\". this goes nowhere and gets saved by fake ellg and his \"no you\" demeanor, changing into \"robot v robot\" combat instead. this kinda is geneaology core LOL"
    "he encounters a wild problem of Haskell Performance Bottleneck, and we explore the gnarly world of arcane debugging."
    "clonk launches the \"density initiative\", which is like a secondary queue of chat message so he doesn't miss any chat in the midst of him being john purple and having like 500 viewers and becoming a millionaire and acting like he don't know nobody. he doesn't finish this and instead struggles with game boy instead."
    "clonk creates an emacs mode for exploring  Discourse  forums, notably  t/suki . this actually turns out to be a great success and you can get it  here  for emacs i think ???"
    "clonk yaps too much and makes purescript thing ??? he finally brings in first guest of the previously mode (me) but also i have no idea whats going on here i was sick as hell (real bedridden style)"
    "with the beginning of the  SUMMER lisp game jam  he introduces a totally different way to create game boy cartridges: NO TOOLING, all FOOLING. he creates an emacs lisp functions that creates gb roms out of assembly. he creates an image to background sprite format thing so that he can put \"\"\"sprites\"\"\" on memory."
    "he showcases his progress in his no tooling game development endeavors, which consists of buzzing constant sound and increasing number and a prompt box. he continues working on this and eventually \"completes\" it. you can play various clonkhead's lisp game jam entries:"
    "officially the finale of LCOLONQ 2.X, we return INTJ stare, live reactions and various ask friend prompts, creates wasp-prod.el  (whos the elisp file now geiser) , and worked on fixing oub pathfinding. The next TWO sessions are skipped as he has turned into ashes and transported into the astral plane, as in he took a plane to OFFKAI. he returns with a special \"wednesday sesh\"."
    "Clonk has realized the importance of balance between the disruption and the locking in, and he wishes to finish the videogame and other irons upon the forge. will he succeed?"
    "LCOLONQ revisits the idea of a MMO game jam from the beginning of season 2 and discusses scarcity of item, transaction, value, exploits, and Double Spend Problem. he ends up reinventing NFT and OpenSea again somehow and becomes a cryptobro REAL. we somehow do go into 6 hours into MUD discussion never to return again"
    "clonk clones Seraph, LeadenGin gets equity, we implement Sand Mode, and then we cheat in  La Mulana  (cool game). he is also insanely high bpm and briefly crashes even. clonk approaches the limits of the thinkpad and vows for a guix machine two pc setup (he has not done that), the fog is coming..."
    "clonk spreads the joy of being a \"Good Liver\". the actual content is oub but like the birth of Von Vivant is imporatnt"
    "twitch introduces the new Power Up feature that lets you pay money to streamer to post big emojis and distract the screen. this is used extensively by ellg as a result. newspaper temporarily goes down, bugSegz is a thing now, we have some wild OBS moment where all the toggles turn on at once, 'critical-hit bites the bullet (f). we do more oub"
    "clonk introduces the concept of GBA modding and making a compiler for it, and then everything suddenly stops working but this time not because of him but because of twitch. some kind of early deployment crashed sammi and like all of us and we were full on doomsday mode for a while. it was fucked."
    "Clonk plays elden ring DLC (i wasnt there i was streaming)"
    "prod fucking dies and we ooze in commemoration, we also talk about extensive hours of the Eld DLC, and we make some progress on the gba zone. maude is out for relative meeting and clonk's despair begins."
    "clonks pc finally could not handle it and crashes REAL style, allowing all users to double boost scam. this causes local modclonk to be firmly be in behind gendude in the chase for max boostage. we also oub i think"
    "ANOTHER CRASH OCCURS btw, and double boosts happen again. that aside the BulletML saga begins as clonk decides to write the overlay software for the bullet redeem. he adds basic messaging system and searches for options for having transparent passthrough window. a rare Gamba Event also occurs and everyone becomes extremely competitive, only to end in a refund (lame) due to the goals not being founded correctly."
    "the BulletML saga continues as clonk writes the actual parser for bullet. we struggle and then declare victory lol (but then he codes the entire thing off stream in like a day bc lol, duress real)"
    "the saga concludes here officially, as we create the win conditions and clear up various stuff. redeems are made through  the other gizmo in this website , and it relatively works as we display cock and balls on screen."
    "all the backed up redemption gets redeemed today, resulting in like 6 new equity lords and 7 new clones. we then do some oub. we also showcase new starting soon screen"
    "clonk reveals his new obsession of the Song of Ice and Fire Wiki (specifically the wiki), we clone the newest clone of Vettle, we then do some oub."
    "clonk is OUT of power for like days now, and he decides its time he go back to the basics. he talks about how themeing is more important than the docket and talks about how friend needs to do more (he does not do this). we then create a better autocomplete for emacs that are almost complete now. i think. we look into various things."
    "cloud strike happened and we comment on this a lot, we have a lot of programming (emacs, nix, haskell) related discussions (do some classic orange site reading), and then we do some 3do (we got mr green on there, and recieved input via controller. the plan is to render the puppet using the controller input. this thread is planned to continue (abyss)). i think we also had a gamba event? idk."
    "we visit the Twitch Game Jam and how it is fucked up in 9 different layers, we talk about cycling out the bottom bar, gives streaming advice to people, and then do swear word challenge which he gifts 3 subs and gives up. there is also a HUGE gambling event with like half a mill on the line on modclonk sleeping or waking up, and believers gain huge amounts of equity. to follow this up, ellg is crowned moderator status to carry out further gamblation, and it becomes a staple soon after."
    "in terms of actual work, we kill rats for the awesome subscription alert of  \"Hey there! Thank you so much for the subscription! That's really cool of you to do i hope your I hope you are having a really a really nice day today thats that's.. thank thank you, Big, big Ups! Big Dog, Hell yeah! Thank you, thank you again to, for the, for comin in with the big, the Big, the Big Subscription! The Big S! The Big S! i'd like to call it. The Big, the, ei, eigh, gh, hell yeah. Subscription...MUCH??? Yeah... Thank you, thank you again to... for, for that. ao, o, allright. talk to you later!\" . we then add a few more variants and revive the rat back (i think??) and make it happen randomly. we also open submission for sub alerts and we add SAM guy thing and Tyumici thing. SAM is loud and overpowers everything else."
    "we also discover friend's long running issue using the new Emacs Debug Mode we have devised, and experiences life-threatening moments due to clonk suffering various rust lsp moments, vowing to get a new pc (this is still a thread that is ongoing)."
    "Clonk previews and debugs the hands cam (i wasnt there i was streaWhy is this a running theme)"
    "clonk is on the Front Page due to him being a partner and twitch doing the \"ask to be on front page\" thing nowadays. clonk gets like 6000 viewers and the chatterbase constantly changes. gamba happens early on how many things clonk will do in an hour when we experience the peak of fame when clonk yaps straight for like 7 hours and has to extend the session by 3 more hours to barely get half a thing working. we also have hand cam which breaks immediately and reveals that clonk is actually Mr Green Real (he had green gloves with some kind of powder appearantly), and handcam appears occasionally due to hot cumbersome and powdery. modclonk waves once and goes to sleep immediate."
    "in terms of works, clonk brings up the idea of \"hexes\" that can be casted on chatters that does an action based on the bits we have done previously, ideas such as emote swaps, pig latin, ai translation, chatter fusion and such are brought up. we get the really basic hexes working."
    "we also gets gifted 100 subs by chatter eudemoniac and it promptly rat swarms the clonk pc, killing the stream and gendude gets another W over modclonk in the war of boosts. mickynoon soon gifts 100 subs too and gets mega scammed."
    "overall clonk experiences Overflowing in Clout and Acting like he Don't Know Nobody, and it was pretty neat. he promises he can read all the chats and still be (para)social while having thousands of viewers."
    "This season firmly grounds the changes that was slowly evident (we obliterate the docket and do variety contents every stream), we have weddings, twitch \"con\", doing the opposite of what he plans to, im not sure what the throughline is but the two major projects are the \"GBA\" and oub. will he finish this?"
    "Clonk comments on the finale and finishes the hex style."
    "courtesy of  text wall  person, we have sprites for NetHack, and we make an emacs thing for nethack."
    "clonk talks about his fear of cyclones and the sky, and attempts to use Butano, a already built gba modern engine thing. he struggles with the engine-like nature and decides to NIH hell, thus reviving the \"UDC\" era."
    "clonk plans to do Bless again but idk where it went ngl ok so im going to be real i started actually sleeping at my appropriate time around this time so i generally dont know what clonk is doing until the vods come out now, like its actually over i am disqualified as a librarian or sotn, dude someone need to take my mantle and get this page going for me, ill give you the page swear"
    "clonk purchases  GCP3.net  and develops a factor (programming language) thing for web there. he manages to build the core and does not do the rest (actually hook it up to data points). currently the site just cycles between colors every refresh."
    "clonk LOCKs IN to some Haxe project abandoning every previous plans, trying to build a Haxe binding for emacs/elisp, he ultimately loses this battle as he has no prior experience to Haxe, and vows to return."
    "this is an oub episode (the effort post is 50 this time)"
    "we suddenly return to the Pal Emerald zone as we develop a pokemon map editor for emacs. we succeed in doing so and can edit blocks with keyboard."
    "the first of many \"GBA UDC\" episodes to come, clonk develops some part of the GBA system that im not entirely aware of yet"
    "second \"GBA UDC\" episode. he also shares about his video progress i think (he is working on a Game Man youtube video), i think around this point clonk also reverses the Gamer curse? so it is now back to being no counter."
    "clonk sudddenly brings up Wave Function Collapse (algorithm) and using it to make a better level generation for oub, he does not succeed in this and vows to fix this off stream when he comes back. this friday has NO session due to Wedding Sunday. (wild)"
    "Promised to happen, has not happened yet."))

(defun w/glossary-record (inp)
  "Add INP to the glossary input for this stream."
  (push inp w/glossary-inputs))

(defun w/glossary-entry (k)
  "Given the current glossary inputs, pass this stream's entry to K."
  (w/ai
   (s-concat
    "This stream's events:\n"
    (s-join "\n" w/glossary-inputs))
   (lambda (d)
     (funcall k d))
   (s-concat
    "prodzpod's past descriptions:\n"
    (s-join "\n" w/glossary-examples)
    "\nYou are roleplaying as prodzpod, who writes descriptions of sessions in a unique way. I have provided a list of past descriptions written by prodzpod alongside events that happened during a session. Please write a description based on those events in exactly the style of prodzpod. It should be the length of one line in the provided descriptions approximately. Please try hard to closely match the examples. Match the examples VERY closely do not be creative. Do not use purple prose, write exactly as prodzpod did. Do not use adjectives or metaphorical speech or puns or humor. You write in all lowercase."
    )
   ))

(defun w/glossary-save ()
  "Save the glossary entry for the current stream."
  (w/glossary-entry
   (lambda (d)
     (f-write-text d 'utf-8 (f-join (w/asset "glossary") (format-time-string "%Y-%m-%d.txt" (current-time)))))))

(provide 'wasp-glossary)
;;; wasp-glossary.el ends here
