


#' Generate Mnemonic Seed Phrase
#'
#' @returns
#' 1
#' @export
#'
#' @examples
#' 1
gen.mnemonic.seed <- function() {

  # Following this:
  # https://docs.getmonero.org/mnemonics/legacy/
  # More info:
  # https://monero.stackexchange.com/questions/4101/can-you-choose-the-mnemonic-seed-to-create-a-wallet
  # https://monero.stackexchange.com/questions/874/what-is-the-checksum-at-the-end-of-a-mnemonic-seed
  # https://monero.stackexchange.com/questions/7125/why-limit-the-crc-by-a-modulo-of-24-rather-than-1626
  # https://monero-python.readthedocs.io/en/latest/seed.html
  # https://github.com/tari-project/monero-address-creator

  # From https://stackoverflow.com/questions/63273501/converting-hex-string-to-a-64-bit-integer-timestamp-using-r
  hex_to_int64 <- function(hex) {
    # remove eventual 0x prefix
    hex <- gsub("^0x", "", hex)
    if (length(hex) == 1) hex <- list(hex)

    # helper function to convert hex to list of bytes
    to_bytes <- function(x) {
      if (length(x) > 1 || is.list(x)) return(lapply(x, to_bytes))
      y <- strsplit(x, "")[[1]]
      sapply(seq(1, length(y), by = 2), function(i) {
        as.raw(paste0("0x", paste(y[i:(i + 1)], collapse = "")))
      })
    }
    bytes <- to_bytes(hex)

    # unfortunately the conversion to character is needed... otherwise a numeric is returned
    sapply(bytes, function(x) {
      v <- bit64::as.integer64(0)
      for (b in x) v <- 256 * v + as.integer(b)
      as.character(v)
    }) |> bit64::as.integer64()
  }

  # Testing, based on https://docs.getmonero.org/mnemonics/legacy/
  test.digest <- digest::digest("lusbagstamicimivilganeffstrdiftogvaipucroppansholiamoimemsorsynketswedeh",
    algo = "crc32", serialize = FALSE)

  hex_to_int64(test.digest)
  # Want: 2248614488

  hex_to_int64(test.digest) %% 24
  # Want: 8


  # From https://github.com/monero-project/monero/blob/master/src/mnemonics/english.h
  wordlist <-
    c("abbey", "abducts", "ability", "ablaze", "abnormal", "abort",
      "abrasive", "absorb", "abyss", "academy", "aces", "aching", "acidic",
      "acoustic", "acquire", "across", "actress", "acumen", "adapt",
      "addicted", "adept", "adhesive", "adjust", "adopt", "adrenalin",
      "adult", "adventure", "aerial", "afar", "affair", "afield", "afloat",
      "afoot", "afraid", "after", "against", "agenda", "aggravate",
      "agile", "aglow", "agnostic", "agony", "agreed", "ahead", "aided",
      "ailments", "aimless", "airport", "aisle", "ajar", "akin", "alarms",
      "album", "alchemy", "alerts", "algebra", "alkaline", "alley",
      "almost", "aloof", "alpine", "already", "also", "altitude", "alumni",
      "always", "amaze", "ambush", "amended", "amidst", "ammo", "amnesty",
      "among", "amply", "amused", "anchor", "android", "anecdote",
      "angled", "ankle", "annoyed", "answers", "antics", "anvil", "anxiety",
      "anybody", "apart", "apex", "aphid", "aplomb", "apology", "apply",
      "apricot", "aptitude", "aquarium", "arbitrary", "archer", "ardent",
      "arena", "argue", "arises", "army", "around", "arrow", "arsenic",
      "artistic", "ascend", "ashtray", "aside", "asked", "asleep",
      "aspire", "assorted", "asylum", "athlete", "atlas", "atom", "atrium",
      "attire", "auburn", "auctions", "audio", "august", "aunt", "austere",
      "autumn", "avatar", "avidly", "avoid", "awakened", "awesome",
      "awful", "awkward", "awning", "awoken", "axes", "axis", "axle",
      "aztec", "azure", "baby", "bacon", "badge", "baffles", "bagpipe",
      "bailed", "bakery", "balding", "bamboo", "banjo", "baptism",
      "basin", "batch", "bawled", "bays", "because", "beer", "befit",
      "begun", "behind", "being", "below", "bemused", "benches", "berries",
      "bested", "betting", "bevel", "beware", "beyond", "bias", "bicycle",
      "bids", "bifocals", "biggest", "bikini", "bimonthly", "binocular",
      "biology", "biplane", "birth", "biscuit", "bite", "biweekly",
      "blender", "blip", "bluntly", "boat", "bobsled", "bodies", "bogeys",
      "boil", "boldly", "bomb", "border", "boss", "both", "bounced",
      "bovine", "bowling", "boxes", "boyfriend", "broken", "brunt",
      "bubble", "buckets", "budget", "buffet", "bugs", "building",
      "bulb", "bumper", "bunch", "business", "butter", "buying", "buzzer",
      "bygones", "byline", "bypass", "cabin", "cactus", "cadets", "cafe",
      "cage", "cajun", "cake", "calamity", "camp", "candy", "casket",
      "catch", "cause", "cavernous", "cease", "cedar", "ceiling", "cell",
      "cement", "cent", "certain", "chlorine", "chrome", "cider", "cigar",
      "cinema", "circle", "cistern", "citadel", "civilian", "claim",
      "click", "clue", "coal", "cobra", "cocoa", "code", "coexist",
      "coffee", "cogs", "cohesive", "coils", "colony", "comb", "cool",
      "copy", "corrode", "costume", "cottage", "cousin", "cowl", "criminal",
      "cube", "cucumber", "cuddled", "cuffs", "cuisine", "cunning",
      "cupcake", "custom", "cycling", "cylinder", "cynical", "dabbing",
      "dads", "daft", "dagger", "daily", "damp", "dangerous", "dapper",
      "darted", "dash", "dating", "dauntless", "dawn", "daytime", "dazed",
      "debut", "decay", "dedicated", "deepest", "deftly", "degrees",
      "dehydrate", "deity", "dejected", "delayed", "demonstrate", "dented",
      "deodorant", "depth", "desk", "devoid", "dewdrop", "dexterity",
      "dialect", "dice", "diet", "different", "digit", "dilute", "dime",
      "dinner", "diode", "diplomat", "directed", "distance", "ditch",
      "divers", "dizzy", "doctor", "dodge", "does", "dogs", "doing",
      "dolphin", "domestic", "donuts", "doorway", "dormant", "dosage",
      "dotted", "double", "dove", "down", "dozen", "dreams", "drinks",
      "drowning", "drunk", "drying", "dual", "dubbed", "duckling",
      "dude", "duets", "duke", "dullness", "dummy", "dunes", "duplex",
      "duration", "dusted", "duties", "dwarf", "dwelt", "dwindling",
      "dying", "dynamite", "dyslexic", "each", "eagle", "earth", "easy",
      "eating", "eavesdrop", "eccentric", "echo", "eclipse", "economics",
      "ecstatic", "eden", "edgy", "edited", "educated", "eels", "efficient",
      "eggs", "egotistic", "eight", "either", "eject", "elapse", "elbow",
      "eldest", "eleven", "elite", "elope", "else", "eluded", "emails",
      "ember", "emerge", "emit", "emotion", "empty", "emulate", "energy",
      "enforce", "enhanced", "enigma", "enjoy", "enlist", "enmity",
      "enough", "enraged", "ensign", "entrance", "envy", "epoxy", "equip",
      "erase", "erected", "erosion", "error", "eskimos", "espionage",
      "essential", "estate", "etched", "eternal", "ethics", "etiquette",
      "evaluate", "evenings", "evicted", "evolved", "examine", "excess",
      "exhale", "exit", "exotic", "exquisite", "extra", "exult", "fabrics",
      "factual", "fading", "fainted", "faked", "fall", "family", "fancy",
      "farming", "fatal", "faulty", "fawns", "faxed", "fazed", "feast",
      "february", "federal", "feel", "feline", "females", "fences",
      "ferry", "festival", "fetches", "fever", "fewest", "fiat", "fibula",
      "fictional", "fidget", "fierce", "fifteen", "fight", "films",
      "firm", "fishing", "fitting", "five", "fixate", "fizzle", "fleet",
      "flippant", "flying", "foamy", "focus", "foes", "foggy", "foiled",
      "folding", "fonts", "foolish", "fossil", "fountain", "fowls",
      "foxes", "foyer", "framed", "friendly", "frown", "fruit", "frying",
      "fudge", "fuel", "fugitive", "fully", "fuming", "fungal", "furnished",
      "fuselage", "future", "fuzzy", "gables", "gadget", "gags", "gained",
      "galaxy", "gambit", "gang", "gasp", "gather", "gauze", "gave",
      "gawk", "gaze", "gearbox", "gecko", "geek", "gels", "gemstone",
      "general", "geometry", "germs", "gesture", "getting", "geyser",
      "ghetto", "ghost", "giant", "giddy", "gifts", "gigantic", "gills",
      "gimmick", "ginger", "girth", "giving", "glass", "gleeful", "glide",
      "gnaw", "gnome", "goat", "goblet", "godfather", "goes", "goggles",
      "going", "goldfish", "gone", "goodbye", "gopher", "gorilla",
      "gossip", "gotten", "gourmet", "governing", "gown", "greater",
      "grunt", "guarded", "guest", "guide", "gulp", "gumball", "guru",
      "gusts", "gutter", "guys", "gymnast", "gypsy", "gyrate", "habitat",
      "hacksaw", "haggled", "hairy", "hamburger", "happens", "hashing",
      "hatchet", "haunted", "having", "hawk", "haystack", "hazard",
      "hectare", "hedgehog", "heels", "hefty", "height", "hemlock",
      "hence", "heron", "hesitate", "hexagon", "hickory", "hiding",
      "highway", "hijack", "hiker", "hills", "himself", "hinder", "hippo",
      "hire", "history", "hitched", "hive", "hoax", "hobby", "hockey",
      "hoisting", "hold", "honked", "hookup", "hope", "hornet", "hospital",
      "hotel", "hounded", "hover", "howls", "hubcaps", "huddle", "huge",
      "hull", "humid", "hunter", "hurried", "husband", "huts", "hybrid",
      "hydrogen", "hyper", "iceberg", "icing", "icon", "identity",
      "idiom", "idled", "idols", "igloo", "ignore", "iguana", "illness",
      "imagine", "imbalance", "imitate", "impel", "inactive", "inbound",
      "incur", "industrial", "inexact", "inflamed", "ingested", "initiate",
      "injury", "inkling", "inline", "inmate", "innocent", "inorganic",
      "input", "inquest", "inroads", "insult", "intended", "inundate",
      "invoke", "inwardly", "ionic", "irate", "iris", "irony", "irritate",
      "island", "isolated", "issued", "italics", "itches", "items",
      "itinerary", "itself", "ivory", "jabbed", "jackets", "jaded",
      "jagged", "jailed", "jamming", "january", "jargon", "jaunt",
      "javelin", "jaws", "jazz", "jeans", "jeers", "jellyfish", "jeopardy",
      "jerseys", "jester", "jetting", "jewels", "jigsaw", "jingle",
      "jittery", "jive", "jobs", "jockey", "jogger", "joining", "joking",
      "jolted", "jostle", "journal", "joyous", "jubilee", "judge",
      "juggled", "juicy", "jukebox", "july", "jump", "junk", "jury",
      "justice", "juvenile", "kangaroo", "karate", "keep", "kennel",
      "kept", "kernels", "kettle", "keyboard", "kickoff", "kidneys",
      "king", "kiosk", "kisses", "kitchens", "kiwi", "knapsack", "knee",
      "knife", "knowledge", "knuckle", "koala", "laboratory", "ladder",
      "lagoon", "lair", "lakes", "lamb", "language", "laptop", "large",
      "last", "later", "launching", "lava", "lawsuit", "layout", "lazy",
      "lectures", "ledge", "leech", "left", "legion", "leisure", "lemon",
      "lending", "leopard", "lesson", "lettuce", "lexicon", "liar",
      "library", "licks", "lids", "lied", "lifestyle", "light", "likewise",
      "lilac", "limits", "linen", "lion", "lipstick", "liquid", "listen",
      "lively", "loaded", "lobster", "locker", "lodge", "lofty", "logic",
      "loincloth", "long", "looking", "lopped", "lordship", "losing",
      "lottery", "loudly", "love", "lower", "loyal", "lucky", "luggage",
      "lukewarm", "lullaby", "lumber", "lunar", "lurk", "lush", "luxury",
      "lymph", "lynx", "lyrics", "macro", "madness", "magically", "mailed",
      "major", "makeup", "malady", "mammal", "maps", "masterful", "match",
      "maul", "maverick", "maximum", "mayor", "maze", "meant", "mechanic",
      "medicate", "meeting", "megabyte", "melting", "memoir", "menu",
      "merger", "mesh", "metro", "mews", "mice", "midst", "mighty",
      "mime", "mirror", "misery", "mittens", "mixture", "moat", "mobile",
      "mocked", "mohawk", "moisture", "molten", "moment", "money",
      "moon", "mops", "morsel", "mostly", "motherly", "mouth", "movement",
      "mowing", "much", "muddy", "muffin", "mugged", "mullet", "mumble",
      "mundane", "muppet", "mural", "musical", "muzzle", "myriad",
      "mystery", "myth", "nabbing", "nagged", "nail", "names", "nanny",
      "napkin", "narrate", "nasty", "natural", "nautical", "navy",
      "nearby", "necklace", "needed", "negative", "neither", "neon",
      "nephew", "nerves", "nestle", "network", "neutral", "never",
      "newt", "nexus", "nibs", "niche", "niece", "nifty", "nightly",
      "nimbly", "nineteen", "nirvana", "nitrogen", "nobody", "nocturnal",
      "nodes", "noises", "nomad", "noodles", "northern", "nostril",
      "noted", "nouns", "novelty", "nowhere", "nozzle", "nuance", "nucleus",
      "nudged", "nugget", "nuisance", "null", "number", "nuns", "nurse",
      "nutshell", "nylon", "oaks", "oars", "oasis", "oatmeal", "obedient",
      "object", "obliged", "obnoxious", "observant", "obtains", "obvious",
      "occur", "ocean", "october", "odds", "odometer", "offend", "often",
      "oilfield", "ointment", "okay", "older", "olive", "olympics",
      "omega", "omission", "omnibus", "onboard", "oncoming", "oneself",
      "ongoing", "onion", "online", "onslaught", "onto", "onward",
      "oozed", "opacity", "opened", "opposite", "optical", "opus",
      "orange", "orbit", "orchid", "orders", "organs", "origin", "ornament",
      "orphans", "oscar", "ostrich", "otherwise", "otter", "ouch",
      "ought", "ounce", "ourselves", "oust", "outbreak", "oval", "oven",
      "owed", "owls", "owner", "oxidant", "oxygen", "oyster", "ozone",
      "pact", "paddles", "pager", "pairing", "palace", "pamphlet",
      "pancakes", "paper", "paradise", "pastry", "patio", "pause",
      "pavements", "pawnshop", "payment", "peaches", "pebbles", "peculiar",
      "pedantic", "peeled", "pegs", "pelican", "pencil", "people",
      "pepper", "perfect", "pests", "petals", "phase", "pheasants",
      "phone", "phrases", "physics", "piano", "picked", "pierce", "pigment",
      "piloted", "pimple", "pinched", "pioneer", "pipeline", "pirate",
      "pistons", "pitched", "pivot", "pixels", "pizza", "playful",
      "pledge", "pliers", "plotting", "plus", "plywood", "poaching",
      "pockets", "podcast", "poetry", "point", "poker", "polar", "ponies",
      "pool", "popular", "portents", "possible", "potato", "pouch",
      "poverty", "powder", "pram", "present", "pride", "problems",
      "pruned", "prying", "psychic", "public", "puck", "puddle", "puffin",
      "pulp", "pumpkins", "punch", "puppy", "purged", "push", "putty",
      "puzzled", "pylons", "pyramid", "python", "queen", "quick", "quote",
      "rabbits", "racetrack", "radar", "rafts", "rage", "railway",
      "raking", "rally", "ramped", "randomly", "rapid", "rarest", "rash",
      "rated", "ravine", "rays", "razor", "react", "rebel", "recipe",
      "reduce", "reef", "refer", "regular", "reheat", "reinvest", "rejoices",
      "rekindle", "relic", "remedy", "renting", "reorder", "repent",
      "request", "reruns", "rest", "return", "reunion", "revamp", "rewind",
      "rhino", "rhythm", "ribbon", "richly", "ridges", "rift", "rigid",
      "rims", "ringing", "riots", "ripped", "rising", "ritual", "river",
      "roared", "robot", "rockets", "rodent", "rogue", "roles", "romance",
      "roomy", "roped", "roster", "rotate", "rounded", "rover", "rowboat",
      "royal", "ruby", "rudely", "ruffled", "rugged", "ruined", "ruling",
      "rumble", "runway", "rural", "rustled", "ruthless", "sabotage",
      "sack", "sadness", "safety", "saga", "sailor", "sake", "salads",
      "sample", "sanity", "sapling", "sarcasm", "sash", "satin", "saucepan",
      "saved", "sawmill", "saxophone", "sayings", "scamper", "scenic",
      "school", "science", "scoop", "scrub", "scuba", "seasons", "second",
      "sedan", "seeded", "segments", "seismic", "selfish", "semifinal",
      "sensible", "september", "sequence", "serving", "session", "setup",
      "seventh", "sewage", "shackles", "shelter", "shipped", "shocking",
      "shrugged", "shuffled", "shyness", "siblings", "sickness", "sidekick",
      "sieve", "sifting", "sighting", "silk", "simplest", "sincerely",
      "sipped", "siren", "situated", "sixteen", "sizes", "skater",
      "skew", "skirting", "skulls", "skydive", "slackens", "sleepless",
      "slid", "slower", "slug", "smash", "smelting", "smidgen", "smog",
      "smuggled", "snake", "sneeze", "sniff", "snout", "snug", "soapy",
      "sober", "soccer", "soda", "software", "soggy", "soil", "solved",
      "somewhere", "sonic", "soothe", "soprano", "sorry", "southern",
      "sovereign", "sowed", "soya", "space", "speedy", "sphere", "spiders",
      "splendid", "spout", "sprig", "spud", "spying", "square", "stacking",
      "stellar", "stick", "stockpile", "strained", "stunning", "stylishly",
      "subtly", "succeed", "suddenly", "suede", "suffice", "sugar",
      "suitcase", "sulking", "summon", "sunken", "superior", "surfer",
      "sushi", "suture", "swagger", "swept", "swiftly", "sword", "swung",
      "syllabus", "symptoms", "syndrome", "syringe", "system", "taboo",
      "tacit", "tadpoles", "tagged", "tail", "taken", "talent", "tamper",
      "tanks", "tapestry", "tarnished", "tasked", "tattoo", "taunts",
      "tavern", "tawny", "taxi", "teardrop", "technical", "tedious",
      "teeming", "tell", "template", "tender", "tepid", "tequila",
      "terminal", "testing", "tether", "textbook", "thaw", "theatrics",
      "thirsty", "thorn", "threaten", "thumbs", "thwart", "ticket",
      "tidy", "tiers", "tiger", "tilt", "timber", "tinted", "tipsy",
      "tirade", "tissue", "titans", "toaster", "tobacco", "today",
      "toenail", "toffee", "together", "toilet", "token", "tolerant",
      "tomorrow", "tonic", "toolbox", "topic", "torch", "tossed", "total",
      "touchy", "towel", "toxic", "toyed", "trash", "trendy", "tribal",
      "trolling", "truth", "trying", "tsunami", "tubes", "tucks", "tudor",
      "tuesday", "tufts", "tugs", "tuition", "tulips", "tumbling",
      "tunnel", "turnip", "tusks", "tutor", "tuxedo", "twang", "tweezers",
      "twice", "twofold", "tycoon", "typist", "tyrant", "ugly", "ulcers",
      "ultimate", "umbrella", "umpire", "unafraid", "unbending", "uncle",
      "under", "uneven", "unfit", "ungainly", "unhappy", "union", "unjustly",
      "unknown", "unlikely", "unmask", "unnoticed", "unopened", "unplugs",
      "unquoted", "unrest", "unsafe", "until", "unusual", "unveil",
      "unwind", "unzip", "upbeat", "upcoming", "update", "upgrade",
      "uphill", "upkeep", "upload", "upon", "upper", "upright", "upstairs",
      "uptight", "upwards", "urban", "urchins", "urgent", "usage",
      "useful", "usher", "using", "usual", "utensils", "utility", "utmost",
      "utopia", "uttered", "vacation", "vague", "vain", "value", "vampire",
      "vane", "vapidly", "vary", "vastness", "vats", "vaults", "vector",
      "veered", "vegan", "vehicle", "vein", "velvet", "venomous", "verification",
      "vessel", "veteran", "vexed", "vials", "vibrate", "victim", "video",
      "viewpoint", "vigilant", "viking", "village", "vinegar", "violin",
      "vipers", "virtual", "visited", "vitals", "vivid", "vixen", "vocal",
      "vogue", "voice", "volcano", "vortex", "voted", "voucher", "vowels",
      "voyage", "vulture", "wade", "waffle", "wagtail", "waist", "waking",
      "wallets", "wanted", "warped", "washing", "water", "waveform",
      "waxing", "wayside", "weavers", "website", "wedge", "weekday",
      "weird", "welders", "went", "wept", "were", "western", "wetsuit",
      "whale", "when", "whipped", "whole", "wickets", "width", "wield",
      "wife", "wiggle", "wildly", "winter", "wipeout", "wiring", "wise",
      "withdrawn", "wives", "wizard", "wobbly", "woes", "woken", "wolf",
      "womanly", "wonders", "woozy", "worry", "wounded", "woven", "wrap",
      "wrist", "wrong", "yacht", "yahoo", "yanks", "yard", "yawning",
      "yearbook", "yellow", "yesterday", "yeti", "yields", "yodel",
      "yoga", "younger", "yoyo", "zapped", "zeal", "zebra", "zero",
      "zesty", "zigzags", "zinger", "zippers", "zodiac", "zombie",
      "zones", "zoom")

  # data.table::uniqueN(cut(runif(1000000), breaks = seq(0, 1, by = 1/length(wordlist))))
  # table(cut(runif(1000000), breaks = seq(0, 1, by = 1/length(wordlist)), labels = FALSE))

  # qbirthday(classes = length(wordlist)^2)

  selected.word.indices <- cut(openssl::rand_num(24),
    breaks = seq(0, 1, by = 1/length(wordlist)), labels = FALSE)
  # https://cran.r-project.org/web/packages/openssl/vignettes/secure_rng.html
  # Alternative:
  # https://coolbutuseless.r-universe.dev/cryptorng/doc/readme

  selected.words <- wordlist[selected.word.indices]

  selected.words.trunc.3 <- substr(selected.words, 1, 3)

  crc32.digest <- digest::digest(paste0(selected.words.trunc.3, collapse = ""), algo = "crc32", serialize = FALSE)
  # Must use serialize = FALSE because we want just the string contents, not the
  # R object that holds the string

  checksum.word.index <- as.integer(1 + hex_to_int64(crc32.digest) %% 24)
  # Add 1 because R is 1-indexed
  # as.integer() to convert from 64-bit to 32-bit for indexing

  selected.words.25 <- c(selected.words, selected.words[checksum.word.index])

  selected.words.25

}




gen.wallet.dir <- function(id, mnemonic_seed) {
  if (length(mnemonic_seed) != 25) {
    stop("Length of mnemonic_seed is not 25.")
  }
  paste0(formatC(id, width = 3, flag = "0"), "_",
    paste0(mnemonic_seed[1:2], collapse = "_"))
}




#' Generate wallets
#'
#' @param monerod.rpc.port monerod.rpc.port
#' @param n.wallets n.wallets
#' @param wallets.data.file wallets.data.file
#' @param print.json print.json
#' @param cleanup.process.monero_wallet_rpc cleanup.process.monero_wallet_rpc
#'
#' @returns
#' 1
#' @export
#'
#' @examples
#' 1
gen.wallets <- function(monerod.rpc.port,
  n.wallets,
  wallets.data.file = "spam_wallets_data.rds",
  print.json = TRUE,
  cleanup.process.monero_wallet_rpc = FALSE) {

  # https://monero.stackexchange.com/questions/11043/how-to-generate-a-monero-wallet-programatically

  # ./monero-wallet-rpc --testnet --trusted-daemon --disable-rpc-ban --password "" --disable-rpc-login --wallet-dir . --daemon-address 127.0.0.1:78081  --rpc-bind-port 7676

  # paste0('./monero-wallet-rpc --testnet --trusted-daemon --disable-rpc-ban --password "" --disable-rpc-login --wallet-dir . --daemon-address ', 127.0.0.1:78081  , ' --rpc-bind-port ', 7676)

  stopifnot(length(n.wallets) == 1 && is.numeric(n.wallets) && n.wallets > 0)

  monero_wallet_rpc.binary <- ifelse(.Platform$OS.type == "windows",
    "monero-wallet-rpc.exe", "./monero-wallet-rpc")

  if ( ! (file.exists("monero-wallet-rpc") | file.exists("monero-wallet-rpc.exe") ) ) {
    stop("monero-wallet-rpc binary not in current working directory.")
  }

  wallets <- vector("list", n.wallets)

  for (id in seq_len(n.wallets)) {

    wallets[[id]][["wallet_id"]] <- id

    wallets[[id]][["mnemonic_seed"]] <- gen.mnemonic.seed()

    current.height <- xmr.rpc(paste0("http://127.0.0.1:", monerod.rpc.port, "/json_rpc"),
      method = "get_last_block_header")$result$block_header$height

    stopifnot(length(current.height) > 0)

    wallets[[id]][["restore_height"]] <- as.integer(current.height)

    wallets[[id]][["monero_wallet_rpc_port"]] <- parallelly::freePort()

    wallets[[id]][["wallet_dir"]] <- gen.wallet.dir(id, wallets[[id]][["mnemonic_seed"]])

    # https://monero.stackexchange.com/questions/11043/how-to-generate-a-monero-wallet-programatically

    monero.wallet.rpc.startup.flags <- c(
      "--testnet",
      paste0("--rpc-bind-port=", wallets[[id]][["monero_wallet_rpc_port"]]),
      paste0("--wallet-dir=", wallets[[id]][["wallet_dir"]]),
      paste0("--daemon-address=", "127.0.0.1:", monerod.rpc.port),
      paste0("--log-file=", wallets[[id]][["wallet_dir"]], "/monero-wallet-rpc.log"),
      "--trusted-daemon", "--disable-rpc-ban", "--disable-rpc-login"
    )
    # Separate args into elements in a vector. No spaces. Use "=" instead of space.

    monero.wallet.rpc.process <- processx::process$new(monero_wallet_rpc.binary,
      monero.wallet.rpc.startup.flags, cleanup = cleanup.process.monero_wallet_rpc)
    # Note: No space after command or it will error

    wallets[[id]][["monero_wallet_rpc_process"]] <-
      monero.wallet.rpc.process

    wallets[[id]][["monero_wallet_rpc_pid"]] <- monero.wallet.rpc.process$get_pid()

    Sys.sleep(10) # Wait 10 seconds to make sure monero-wallet-rpc has finished booting

    restore_deterministic_wallet.output <- restore_deterministic_wallet(
      wallet_rpc_port = wallets[[id]][["monero_wallet_rpc_port"]],
      wallet_id = id,
      seed = wallets[[id]][["mnemonic_seed"]],
      restore_height = wallets[[id]][["restore_height"]]
    )

    wallets[[id]][["primary_address"]] <-
      restore_deterministic_wallet.output$result$address

    xmr.rpc(url.rpc = paste0("http://127.0.0.1:",
        wallets[[id]][["monero_wallet_rpc_port"]], "/json_rpc"),
      method = "set_subaddress_lookahead",
      params = list(major_idx = 50L, minor_idx = 2L))
    # Setting minor_idx to a low value reduces unnecessary workload
    # https://github.com/monero-project/monero/pull/9953

    save_wallet(wallets[[id]][["monero_wallet_rpc_port"]])

  }

  for (id in seq_len(n.wallets)) {

    filename <- paste0(formatC(id, width = 3, flag = "0"), "_",
      paste0(wallets[[id]][["mnemonic_seed"]][1:2], collapse = "_"))

    json.data <- list(
      wallet_dir = wallets[[id]][["wallet_dir"]],
      address = wallets[[id]][["primary_address"]],
      seed = wallets[[id]][["mnemonic_seed"]],
      restore_height = wallets[[id]][["restore_height"]],
      monero_wallet_rpc_port = wallets[[id]][["monero_wallet_rpc_port"]],
      monero_wallet_rpc_pid = wallets[[id]][["monero_wallet_rpc_pid"]]
    )

    wallets[[id]][["json"]] <- json.data

  }

  all.json <- RJSONIO::toJSON(lapply(wallets, FUN = function(x) {x[["json"]]}),
    pretty = TRUE)

  cat(all.json, "\n", file = gsub("[.]rds$", ".json", wallets.data.file))

  if (print.json) {
    cat(all.json, "\n")
  }

  class(wallets) <- "wallets"

  saveRDS(wallets, file = wallets.data.file)

  return(wallets)

}








#' Fund wallets
#'
#' @param x x
#' @param amount amount
#' @param fee.priority fee.priority
#'
#' @returns
#' 1
#' @export
#'
#' @examples
#' 1
fund.wallets <- function(x, amount, fee.priority = "priority") {

  if ( ! ( inherits(x, "wallets") | inherits(x, "character") ) ) {
    stop("x input is of incorrect class")
  }

  if (inherits(x, "wallets")) {
    amount.each <- floor(1e+12 * ( amount / length(x) ) ) / 1e+12
    # Round down to the nearest piconero
    addresses <- sapply(x, function(x) x$primary_address)
  }

  if (inherits(x, "character")) {
    amount.each <- floor(1e+12 * ( amount / length(x) ) ) / 1e+12
    # Round down to the nearest piconero
    # https://www.getmonero.org/resources/moneropedia/denominations.html
    addresses <- x
  }

  command <- paste0("transfer ", fee.priority, " ",
    paste0(addresses, " ", amount.each, collapse = " "), " subtractfeefrom=all")
  # https://monero.stackexchange.com/questions/4021/how-do-i-send-xmr-to-multiple-different-addresses
  # subtractfeefrom feature:
  # https://github.com/monero-project/monero/pull/8861

  cat(command, "\n")

  return(invisible(command))

}










