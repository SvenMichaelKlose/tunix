#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "bdb.h"
#include "symbol.h"

#include "unity.h"

extern bdb symdb;

char *names[] = {
"the", "be", "and", "of", "a", "in", "to", "have", "it", "I",
"that", "for", "you", "he", "with", "on", "do", "say", "this", "they",
"at", "but", "we", "his", "from", "not", "by", "she", "or", "as",
"what", "go", "their", "can", "who", "get", "if", "would", "her", "all",
"my", "make", "about", "know", "will", "as", "up", "one", "time", "there",
"year", "so", "think", "when", "which", "them", "some", "me", "people", "take",
"out", "into", "just", "see", "him", "your", "come", "could", "now", "than",
"like", "other", "how", "then", "its", "our", "two", "more", "these", "want",
"way", "look", "first", "also", "new", "because", "day", "more", "use", "no",
"man", "find", "here", "thing", "give", "many", "well", "only", "those", "tell",
"one", "very", "her", "even", "back", "any", "good", "woman", "through", "us",
"life", "child", "work", "down", "may", "after", "should", "call", "world", "over",
"school", "still", "try", "last", "ask", "need", "too", "feel", "three", "when",
"state", "never", "become", "between", "high", "really", "something", "most", "another", "much",
"family", "own", "leave", "put", "old", "while", "mean", "on", "keep", "student",
"why", "let", "great", "same", "big", "group", "begin", "seem", "country", "help",
"talk", "where", "turn", "problem", "every", "start", "hand", "might", "American", "show",
"part", "about", "against", "place", "over", "such", "again", "few", "case", "most",
"week", "company", "where", "system", "each", "right", "program", "hear", "question", "during",
"play", "government", "run", "small", "number", "off", "always", "move", "night", "live",
"Mr", "point", "believe", "hold", "today", "bring", "happen", "next", "without", "before",
"large", "all", "million", "must", "home", "under", "water", "room", "write", "mother",
"area", "national", "money", "story", "young", "fact", "month", "different", "lot", "study",
"book", "eye", "job", "word", "though", "business", "issue", "side", "kind", "four",
"head", "far", "black", "long", "both", "little", "house", "yes", "after", "since",
"long", "provide", "service", "around", "friend", "important", "father", "sit", "away", "until",
"power", "hour", "game", "often", "yet", "line", "political", "end", "among", "ever",
"stand", "bad", "lose", "however", "member", "pay", "law", "meet", "car", "city",
"almost", "include", "continue", "set", "later", "community", "name", "five", "once", "white",
"least", "president", "learn", "real", "change", "team", "minute", "best", "several", "idea",
"kid", "body", "information", "nothing", "ago", "lead", "social", "understand", "whether", "watch",
"together", "follow", "parent", "stop", "face", "anything", "create", "public", "already", "speak",
"others", "read", "level", "allow", "add", "office", "spend", "door", "health", "person",
"art", "sure", "war", "history", "party", "within", "grow", "result", "open", "morning",
"walk", "reason", "low", "win", "research", "girl", "guy", "early", "food", "before",
"moment", "himself", "air", "teacher", "force", "offer", "enough", "both", "education", "across",
"although", "remember", "foot", "second", "boy", "maybe", "toward", "able", "age", "off",
"policy", "everything", "love", "process", "music", "including", "consider", "appear", "actually", "buy",
"probably", "human", "wait", "serve", "market", "die", "send", "expect", "home", "sense",
"build", "stay", "fall", "oh", "nation", "plan", "cut", "college", "interest", "death",
"course", "someone", "experience", "behind", "reach", "local", "kill", "six", "remain", "effect",
"use", "yeah", "suggest", "class", "control", "raise", "care", "perhaps", "little", "late",
"hard", "field", "else", "pass", "former", "sell", "major", "sometimes", "require", "along",
"development", "themselves", "report", "role", "better", "economic", "effort", "up", "decide", "rate",
"strong", "possible", "heart", "drug", "show", "leader", "light", "voice", "wife", "whole",
"police", "mind", "finally", "pull", "return", "free", "military", "price", "report", "less",
"according", "decision", "explain", "son", "hope", "even", "develop", "view", "relationship", "carry",
"town", "road", "drive", "arm", "true", "federal", "break", "better", "difference", "thank",
"receive", "value", "international", "building", "action", "full", "model", "join", "season", "society",
"because", "tax", "director", "early", "position", "player", "agree", "especially", "record", "pick",
"wear", "paper", "special", "space", "ground", "form", "support", "event", "official", "whose",
"matter", "everyone", "center", "couple", "site", "project", "hit", "base", "activity", "star",
"table", "need", "court", "produce", "eat", "American", "teach", "oil", "half", "situation",
"easy", "cost", "industry", "figure", "street", "image", "itself", "phone", "either", "cover",
"quite", "picture", "clear", "practice", "piece", "land", "recent", "describe", "product", "doctor",
"wall", "patient", "worker", "news", "test", "movie", "certain", "north", "personal", "simply",
"third", "technology", "catch", "step", "baby", "computer", "type", "attention", "draw", "film",
"Republican", "tree", "source", "red", "nearly", "organization", "choose", "cause", "hair", "look",
"point", "century", "evidence", "window", "difficult", "listen", "soon", "culture", "billion", "chance",
"brother", "energy", "period", "course", "summer", "less", "realize", "hundred", "available", "plant",
"likely", "opportunity", "term", "short", "letter", "condition", "choice", "place", "single", "rule",
"daughter", "administration", "south", "husband", "Congress", "floor", "campaign", "material", "population", "well",
"call", "economy", "medical", "hospital", "church", "close", "thousand", "risk", "current", "fire",
"future", "wrong", "involve", "defense", "anyone", "increase", "security", "bank", "myself", "certainly",
"west", "sport", "board", "seek", "per", "subject", "officer", "private", "rest", "behavior",
"deal", "performance", "fight", "throw", "top", "quickly", "past", "goal", "second", "order",
"author", "represent", "focus", "foreign", "drop", "plan", "blood", "agency", "push", "nature",
"color", "no", "recently", "store", "reduce", "sound", "note", "fine", "near", "movement",
"page", "enter", "share", "common", "poor", "natural", "race", "concern", "series", "significant",
"similar", "hot", "language", "each", "usually", "response", "dead", "rise", "animal", "factor",
"decade", "article", "shoot", "east", "save", "seven", "artist", "away", "scene", "stock",
"career", "despite", "central", "eight", "thus", "treatment", "beyond", "happy", "exactly", "protect",
"approach", "lie", "size", "dog", "fund", "serious", "occur", "media", "ready", "sign",
"thought", "list", "individual", "simple", "quality", "pressure", "accept", "answer", "resource", "identify",
"left", "meeting", "determine", "prepare", "disease", "whatever", "success", "argue", "cup", "particularly",
"amount", "ability", "staff", "recognize", "indicate", "character", "growth", "loss", "degree", "wonder",
"attack", "herself", "region", "television", "box", "TV", "training", "pretty", "trade", "deal",
"election", "everybody", "physical", "lay", "general", "feeling", "standard", "bill", "message", "fail",
"outside", "arrive", "analysis", "benefit", "name", "sex", "forward", "lawyer", "present", "section",
"environmental", "glass", "answer", "skill", "sister", "PM", "professor", "operation", "financial", "crime",
"stage", "ok", "compare", "authority", "miss", "design", "sort", "act", "ten", "knowledge",
"gun", "station", "blue", "state", "strategy", "little", "clearly", "discuss", "indeed", "force",
"truth", "song", "example", "democratic", "check", "environment", "leg", "dark", "public", "various",
"rather", "laugh", "guess", "executive", "set", "study", "prove", "hang", "entire", "rock",
"design", "enough", "forget", "since", "claim", "note", "remove", "manager", "help", "close",
"sound", "enjoy", "network", "legal", "religious", "cold", "form", "final", "main", "science",
"green", "memory", "card", "above", "seat", "cell", "establish", "nice", "trial", "expert",
"that", "spring", "firm", "Democrat", "radio", "visit", "management", "care", "avoid", "imagine",
"tonight", "huge", "ball", "finish", "yourself", "theory", "impact", "respond", "statement", "maintain",
"charge", "popular", "traditional", "onto", "reveal", "direction", "weapon", "employee", "cultural", "contain",
"peace", "pain", "apply", "measure", "wide", "shake", "fly", "interview", "manage", "chair",
"fish", "particular", "camera", "structure", "politics", "perform", "bit", "weight", "suddenly", "discover",
"candidate", "top", "production", "treat", "trip", "evening", "affect", "inside", "conference", "unit",
"style", "adult", "worry", "range", "mention", "deep", "edge", "specific", "writer", "trouble",
"necessary", "throughout", "challenge", "fear", "shoulder", "institution", "middle", "sea", "dream", "bar",
"beautiful", "property", "instead", "improve", "stuff", "claim", "warm", "associate", "finally", "wing",
"stick", "title", "besides", "imagine", "everyone", "equal", "adopt", "generation", "conversation", "judge",
"beer", "acknowledge", "afterward", "assess", "bread", "branch", "brown", "candidate", "circle", "concentrate",

    NULL
};

void
find_inserted_symbols (void)
{
    symbol * s;
    char **n;
    for (n = names; *n; n++) {
        s = find_symbol (*n);
        TEST_ASSERT_MESSAGE(s, *n);
    }
}

void
symbol_tests (void)
{
    char **n;
    dbid_t id;

    symbol_init ();
    for (n = names; *n; n++) {
        id = add_symbol (*n, strlen (*n));
        TEST_ASSERT_MESSAGE(id != ERROR, *n);
    }

    find_inserted_symbols ();
    symbol_flush ();
    TEST_ASSERT(symdb.cache_root_keys == 0);
    TEST_ASSERT(symdb.cache_root_ids == 0);
    find_inserted_symbols ();
}

void setUp (void) {}
void tearDown (void) {}

int
main (void)
{
  UnityBegin("test/symbol.c");
  RUN_TEST(symbol_tests, 1);
  return UnityEnd();
}
