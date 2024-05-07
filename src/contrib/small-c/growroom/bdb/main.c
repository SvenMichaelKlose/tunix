#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <strings.h>

#include "bdb.h"
#include "cache.h"
#include "symbol.h"
#include "tree2dot.h"

dbid_t
iter_cache_id (void *n)
{
    return ((cnode *) n)->id;
}

char *
iter_cache_string (void *n)
{
    return ((symbol *) ((cnode *) n)->data)->name;
}

void *
iter_cache_keys_left (void *n)
{
    return ((cnode *) n)->kleft;
}

void *
iter_cache_keys_right (void *n)
{
    return ((cnode *) n)->kright;
}

bdb_iter keyiter = {
    .id     = iter_cache_id,
    .string = iter_cache_string,
    .left   = iter_cache_keys_left,
    .right  = iter_cache_keys_right
};

void
symbol_tests (void)
{
    dbid_t id;
    symbol * s;
    char **n;
    char *name = "Homecoming";
    FILE * dot;

    symbol_init ();

    char *names[] = {
"house", "although", "during", "animal", "understand", "horse", "office", "south", "rate", "per", "each", "practice", "above", "tax", "every", "full", "is", "such", "model", "provide", "least", "hit", "drive", "leader", "dog", "environment", "issue", "necessary", "walk", "couple", "explain", "always", "else", "ten", "there", "task", "help", "few", "create", "knowledge", "start", "week", "might", "concern", "campaign", "next", "upon", "effort", "include", "away", "contain", "pressure", "move", "let", "risk", "head", "certain", "heart", "half", "legal", "run", "degree", "since", "everything", "institution", "behavior", "individual", "forward", "director", "allow", "class", "increase", "nature", "financial", "name", "why", "prove", "remain", "military", "reality", "perhaps", "man", "inside", "record", "relationship", "happy", "character", "measure", "grow", "treat", "actually", "plant", "baby", "city", "pressure", "respond", "sport", "before", "car", "gun", "anyway", "course", "focus", "before", "paper", "know", "visit", "medical", "total", "still", "deal", "just", "identify", "site", "yourself", "late", "rise", "computer", "wife", "spring", "son", "rather", "like", "move", "matter", "easy", "air", "black", "against", "public", "hand", "race", "model", "thought", "worker", "about", "itself", "even", "situation", "season", "activity", "end", "nation", "modern", "music", "series", "director", "hit", "next", "main", "within", "range", "trade", "where", "maintain", "energy", "approach", "energy", "across", "suggest", "increase", "guess", "red", "simply", "site", "push", "analysis", "exactly", "approach", "until", "final", "standard", "course", "reduce", "significant", "research", "out", "disease", "data", "crime", "entire", "clear", "claim", "network", "themselves", "key", "away", "level", "region", "central", "myself", "similar", "who", "onto", "deep", "break", "treatment", "improve", "fall", "defense", "claim", "help", "water", "hotel", "technology", "both", "security", "send", "decision", "present", "speech", "seven", "clear", "ten", "state", "wrong", "box", "movement", "thing", "measure", "wall", "sort", "certainly", "central", "long", "out", "within", "need", "by", "successful", "experience", "describe", "quality", "base", "pattern", "political", "first", "mind", "happy", "ahead", "project", "phone", "as", "daughter", "toward", "approach", "else", "poor", "event", "deep", "purpose", "seek", "especially", "man", "much", "easy", "green", "class", "character", "factor", "off", "administration", "student", "goal", "difficult", "least", "buy", "huge", "energy", "our", "college", "board", "all", "day", "financial", "either", "focus", "set", "prove", "true", "cause", "home", "because", "either", "whose", "minute", "assume", "surface", "film", "benefit", "talk", "press", "join", "around", "discuss", "be", "contain", "ground", "decide", "activity", "along", "foot", "anything", "away", "little", "instead", "nation", "board", "strong", "someone", "opinion", "boy", "management", "sort", "dinner", "degree", "expect", "just", "scene", "value", "under", "the", "amount", "need", "someone", "least", "reveal", "design", "look", "region", "line", "sign", "animal", "position", "pass", "rock", "toward", "player", "hold", "million", "well", "quickly", "treat", "as", "method", "give", "listen", "mention", "base", "even", "who", "private", "standard", "private", "central", "because", "seem", "side", "friend", "form", "play", "language", "along", "wish", "involve", "often", "maintain", "include", "through", "show", "morning", "majority", "against", "develop", "provide", "it", "upon", "account", "receive", "six", "age", "seem", "onto", "set", "even", "charge", "less", "short", "plan", "exist", "yourself", "defense", "determine", "official", "give", "produce", "up", "class", "long", "red", "structure", "court", "even", "series", "rather", "specific", "of", "real", "job", "continue", "east", "training", "some", "huge", "then", "fight", "field", "author", "anything", "success", "product", "up", "cause", "clear", "board", "may", "position", "everyone", "ever", "toward", "million", "doctor", "parent", "before", "everything", "significant", "date", "character", "plant", "report", "expect", "none", "strong", "whatever", "become", "that", "contain", "bring", "five", "standard", "everybody", "prove", "fall", "radio", "exist", "present", "decision", "soon", "north", "subject", "will", "green", "whole", "everyone", "long", "cell", "crime", "happy", "door", "present", "catch", "on", "site", "doctor", "site", "describe",

        NULL
    };
    for (int i = 0; i < 1; i++)
        for (n = names; *n; n++)
            id = add_symbol (names[rand () % (sizeof (names) / sizeof (char *) - 1)], strlen (*n));

    s = bdb_map (&symdb, id);
    printf ("Got symbol \"%s\".\n", s->name);

    s = find_symbol (name);
    if (s)
        printf ("Got symbol \"%s\".\n", s->name);
    else
        printf ("Symbol \"%s\" not found.\n", name);

    dot = fopen ("symbol.dot", "w");
    tree2dot (dot, &keyiter, symdb.cache_root_keys);
    fclose (dot);
    symbol_close ();
}

int
main (int argc, char *argv[])
{
    (void) argc;
    (void) argv;

    symbol_tests ();

    return 0;
}
