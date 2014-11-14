# Peter Norvig's Spelling Corrector
# based off of the python implementation at http://norvig.com/spell-correct.html

# I've attempted to keep this close to the python version, although in some cases
# the python version is tighter because of some syntax that is not available in julia
# (e.g., guards in comprehensions).
#
# The python version, at the time of original submission, is also 5-6x faster than
# the julia version.
#
# @kmsquire, 2013-11-29
#

Pkg.add("DataStructures")

using DataStructures
using HTTPClient.HTTPC

include("../perfutil.jl")

words(text) = eachmatch(r"[a-z]+", lowercase(text))

function train(features)
    model = DefaultDict(AbstractString, Int, 1)
    for f in features
        model[f.match] += 1
    end
    return model
end

const NWORDS = train(words(bytestring(get("http://norvig.com/big.txt").body)))

const alphabet = "abcdefghijklmnopqrstuvwxyz"

function edits1(word::AbstractString)
    splits     = [(word[1:i], word[i+1:end]) for i=0:length(word) ]
    deletes    = ["$a$(b[2:end])"               for (a,b) in splits[1:end-1]]
    transposes = ["$a$(b[2])$(b[1])$(b[3:end])" for (a,b) in splits[1:end-2]]
    replaces   = ["$a$c$(b[2:end])"             for (a,b) in splits[1:end-1], c in alphabet]
    inserts    = ["$a$c$b"                      for (a,b) in splits,          c in alphabet]
    return Set([deletes; transposes; replaces[:]; inserts[:]])
end

function known_edits2(word::AbstractString)
    xs = Set{AbstractString}()
    for e1 in edits1(word)
        for e2 in edits1(e1)
            haskey(NWORDS, e2) && push!(xs, e2)
        end
    end
    xs
end

function known(words)
    xs = Set{AbstractString}()
    for word in words
        haskey(NWORDS, word) && push!(xs, word)
    end
    xs
end

function correct(word::AbstractString)
    candidates = known([word])
    length(candidates) == 0 && (candidates = known(edits1(word)))
    length(candidates) == 0 && (candidates = known_edits2(word) )
    length(candidates) == 0 && (candidates = Set([word])        )

    maximum(x->(get(NWORDS, x, 0),x), candidates)[2]
end

################ Testing code from here on ################

function spelltest(tests; bias=0, verbose=false)
    n, bad, unknown, start = 0, 0, 0, tic()
    if bias > 0
        for target in keys(tests)
            NWORDS[target] += bias
        end
    end
    for (target,wrongs) in tests
        for wrong in split(wrongs)
            n += 1
            w = correct(wrong)
            if w!=target
                bad += 1
                unknown += !(target in NWORDS)
                if verbose
                    @printf("correct(%s) => %s (%d); expected %s (%d)\n",
                            wrong, w, NWORDS[w], target, NWORDS[target])
                end
            end
        end
    end

    return Dict("bad"=>bad, "n"=>n, "bias"=>bias, "pct"=>int(100. - 100.*bad/n),
            "unknown"=>unknown, "secs"=>toc())
end

const tests1 = [ "access"=> "acess", "accessing"=> "accesing", "accommodation"=>
"accomodation acommodation acomodation", "account"=> "acount", "address"=>
"adress adres", "addressable"=> "addresable", "arranged"=> "aranged arrainged",
"arrangeing"=> "aranging", "arrangement"=> "arragment", "articles"=> "articals",
"aunt"=> "annt anut arnt", "auxiliary"=> "auxillary", "available"=> "avaible",
"awful"=> "awfall afful", "basically"=> "basicaly", "beginning"=> "begining",
"benefit"=> "benifit", "benefits"=> "benifits", "between"=> "beetween", "bicycle"=>
"bicycal bycicle bycycle", "biscuits"=>
"biscits biscutes biscuts bisquits buiscits buiscuts", "built"=> "biult",
"cake"=> "cak", "career"=> "carrer",
"cemetery"=> "cemetary semetary", "centrally"=> "centraly", "certain"=> "cirtain",
"challenges"=> "chalenges chalenges", "chapter"=> "chaper chaphter chaptur",
"choice"=> "choise", "choosing"=> "chosing", "clerical"=> "clearical",
"committee"=> "comittee", "compare"=> "compair", "completely"=> "completly",
"consider"=> "concider", "considerable"=> "conciderable", "contented"=>
"contenpted contende contended contentid", "curtains"=>
"cartains certans courtens cuaritains curtans curtians curtions", "decide"=> "descide", "decided"=>
"descided", "definitely"=> "definately difinately", "definition"=> "defenition",
"definitions"=> "defenitions", "description"=> "discription", "desiccate"=>
"desicate dessicate dessiccate", "diagrammatically"=> "diagrammaticaally",
"different"=> "diffrent", "driven"=> "dirven", "ecstasy"=> "exstacy ecstacy",
"embarrass"=> "embaras embarass", "establishing"=> "astablishing establising",
"experience"=> "experance experiance", "experiences"=> "experances", "extended"=>
"extented", "extremely"=> "extreamly", "fails"=> "failes", "families"=> "familes",
"february"=> "febuary", "further"=> "futher", "gallery"=> "galery gallary gallerry gallrey",
"hierarchal"=> "hierachial", "hierarchy"=> "hierchy", "inconvenient"=>
"inconvienient inconvient inconvinient", "independent"=> "independant independant",
"initial"=> "intial", "initials"=> "inetials inistals initails initals intials",
"juice"=> "guic juce jucie juise juse", "latest"=> "lates latets latiest latist",
"laugh"=> "lagh lauf laught lugh", "level"=> "leval",
"levels"=> "levals", "liaison"=> "liaision liason", "lieu"=> "liew", "literature"=>
"litriture", "loans"=> "lones", "locally"=> "localy", "magnificent"=>
"magnificnet magificent magnifcent magnifecent magnifiscant magnifisent magnificant",
"management"=> "managment", "meant"=> "ment", "minuscule"=> "miniscule",
"minutes"=> "muinets", "monitoring"=> "monitering", "necessary"=>
"neccesary necesary neccesary necassary necassery neccasary", "occurrence"=>
"occurence occurence", "often"=> "ofen offen offten ofton", "opposite"=>
"opisite oppasite oppesite oppisit oppisite opposit oppossite oppossitte", "parallel"=>
"paralel paralell parrallel parralell parrallell", "particular"=> "particulaur",
"perhaps"=> "perhapse", "personnel"=> "personnell", "planned"=> "planed", "poem"=>
"poame", "poems"=> "poims pomes", "poetry"=> "poartry poertry poetre poety powetry",
"position"=> "possition", "possible"=> "possable", "pretend"=>
"pertend protend prtend pritend", "problem"=> "problam proble promblem proplen",
"pronunciation"=> "pronounciation", "purple"=> "perple perpul poarple",
"questionnaire"=> "questionaire", "really"=> "realy relley relly", "receipt"=>
"receit receite reciet recipt", "receive"=> "recieve", "refreshment"=>
"reafreshment refreshmant refresment refressmunt", "remember"=> "rember remeber rememmer rermember",
"remind"=> "remine remined", "scarcely"=> "scarcly scarecly scarely scarsely",
"scissors"=> "scisors sissors", "separate"=> "seperate",
"singular"=> "singulaur", "someone"=> "somone", "sources"=> "sorces", "southern"=>
"southen", "special"=> "speaical specail specal speical", "splendid"=>
"spledid splended splened splended", "standardizing"=> "stanerdizing", "stomach"=>
"stomac stomache stomec stumache", "supersede"=> "supercede superceed", "there"=> "ther",
"totally"=> "totaly", "transferred"=> "transfred", "transportability"=>
"transportibility", "triangular"=> "triangulaur", "understand"=> "undersand undistand",
"unexpected"=> "unexpcted unexpeted unexspected", "unfortunately"=>
"unfortunatly", "unique"=> "uneque", "useful"=> "usefull", "valuable"=> "valubale valuble",
"variable"=> "varable", "variant"=> "vairiant", "various"=> "vairious",
"visited"=> "fisited viseted vistid vistied", "visitors"=> "vistors",
"voluntary"=> "volantry", "voting"=> "voteing", "wanted"=> "wantid wonted",
"whether"=> "wether", "wrote"=> "rote wote"]

@timeit(spelltest(tests1), "spell", "Peter Norvig's spell corrector")
