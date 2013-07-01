macro timeit(ex,name)
    quote
        t = Inf
        for i=1:5
            t = min(t, @elapsed $ex)
        end
        println($name, "\t", t*1000)
    end
end

macro timeit1(ex,name)
    quote
        println($name, "\t", (@elapsed $ex)*1000)
    end
end

srand(1776)  # get more consistent times

require("$JULIA_HOME/../../examples/list.jl")

function listn1n2(n1::Int,n2::Int)
    l1 = Nil{Int}()
    for i=n2:-1:n1
        l1 = Cons{Int}(i,l1)
    end
    l1
end

@timeit listn1n2(1,10^6) "cons    "
gc()

# issue #1211
include("ziggurat.jl")
a = Array(Float64, 1000000)
@timeit randn_zig!(a) "randn_zig"

# issue #950
include("gk.jl")
@timeit gk(350,[0.1]) "gk      "

# issue #942
s = sparse(ones(280,280));
@timeit s*s "sparsemul"

# issue #939
y = [500000:-1:1];
@timeit sortperm(y) "sortperm"

# issue #938
x = 1:600000;
@timeit sparse(x,x,x) "sparserang"

# issue #445
include("stockcorr.jl")
@timeit stockcorr() "stockcorr"

include("bench_eu.jl")
@timeit bench_eu_vec(10000) "bench_eu_vec"
@timeit bench_eu_devec(10000) "bench_eu_devec"

# issue #1163
include("actor_centrality.jl")
@timeit1 actor_centrality() "actorgraph"

# issue #1168
include("laplace.jl")
@timeit1 laplace_vec() "laplace_vec"
@timeit laplace_devec() "laplace_devec"

# issue #1169
include("go_benchmark.jl")
@timeit1 benchmark(10) "go_benchmark"

# issue #3142
include("simplex.jl")
simplexbenchmark()

function cmp_with_func(x::Vector, f::Function)
    count::Int = 0
    for i = 1:length(x)-1
        if f(x[i], x[i+1])
            count += 1
        end
    end
    return count
end

x = randn(200_000)
@timeit (for n in 1:10; count = cmp_with_func(x, isless) end) "funarg  "


function arith_vectorized(b,c,d)
    a = b.*c + d + 1.0
end

len = 1_000_000
b = randn(len)
c = randn(len)
d = randn(len)

@timeit (for n in 1:10; a = arith_vectorized(b,c,d); end) "vectoriz"

open("random.csv","w") do io
    writecsv(io, rand(100000,4))
end

function parsecsv()
    file = EachLine(open("random.csv"))
    for line in file
        line = split(line, ',')
    end
end

@timeit parsecsv() "splitline"

rm("random.csv")

include("json.jl")

_json_data = "{\"web-app\": {
  \"servlet\": [   
    {
      \"servlet-name\": \"cofaxCDS\",
      \"servlet-class\": \"org.cofax.cds.CDSServlet\",
      \"init-param\": {
        \"configGlossary:installationAt\": \"Philadelphia, PA\",
        \"configGlossary:adminEmail\": \"ksm@pobox.com\",
        \"configGlossary:poweredBy\": \"Cofax\",
        \"configGlossary:poweredByIcon\": \"/images/cofax.gif\",
        \"configGlossary:staticPath\": \"/content/static\",
        \"templateProcessorClass\": \"org.cofax.WysiwygTemplate\",
        \"templateLoaderClass\": \"org.cofax.FilesTemplateLoader\",
        \"templatePath\": \"templates\",
        \"templateOverridePath\": \"\",
        \"defaultListTemplate\": \"listTemplate.htm\",
        \"defaultFileTemplate\": \"articleTemplate.htm\",
        \"useJSP\": false,
        \"jspListTemplate\": \"listTemplate.jsp\",
        \"jspFileTemplate\": \"articleTemplate.jsp\",
        \"cachePackageTagsTrack\": 200,
        \"cachePackageTagsStore\": 200,
        \"cachePackageTagsRefresh\": 60,
        \"cacheTemplatesTrack\": 100,
        \"cacheTemplatesStore\": 50,
        \"cacheTemplatesRefresh\": 15,
        \"cachePagesTrack\": 200,
        \"cachePagesStore\": 100,
        \"cachePagesRefresh\": 10,
        \"cachePagesDirtyRead\": 10,
        \"searchEngineListTemplate\": \"forSearchEnginesList.htm\",
        \"searchEngineFileTemplate\": \"forSearchEngines.htm\",
        \"searchEngineRobotsDb\": \"WEB-INF/robots.db\",
        \"useDataStore\": true,
        \"dataStoreClass\": \"org.cofax.SqlDataStore\",
        \"redirectionClass\": \"org.cofax.SqlRedirection\",
        \"dataStoreName\": \"cofax\",
        \"dataStoreDriver\": \"com.microsoft.jdbc.sqlserver.SQLServerDriver\",
        \"dataStoreUrl\": \"jdbc:microsoft:sqlserver://LOCALHOST:1433;DatabaseName=goon\",
        \"dataStoreUser\": \"sa\",
        \"dataStorePassword\": \"dataStoreTestQuery\",
        \"dataStoreTestQuery\": \"SET NOCOUNT ON;select test='test';\",
        \"dataStoreLogFile\": \"/usr/local/tomcat/logs/datastore.log\",
        \"dataStoreInitConns\": 10,
        \"dataStoreMaxConns\": 100,
        \"dataStoreConnUsageLimit\": 100,
        \"dataStoreLogLevel\": \"debug\",
        \"maxUrlLength\": 500}},
    {
      \"servlet-name\": \"cofaxEmail\",
      \"servlet-class\": \"org.cofax.cds.EmailServlet\",
      \"init-param\": {
      \"mailHost\": \"mail1\",
      \"mailHostOverride\": \"mail2\"}},
    {
      \"servlet-name\": \"cofaxAdmin\",
      \"servlet-class\": \"org.cofax.cds.AdminServlet\"},
 
    {
      \"servlet-name\": \"fileServlet\",
      \"servlet-class\": \"org.cofax.cds.FileServlet\"},
    {
      \"servlet-name\": \"cofaxTools\",
      \"servlet-class\": \"org.cofax.cms.CofaxToolsServlet\",
      \"init-param\": {
        \"templatePath\": \"toolstemplates/\",
        \"log\": 1,
        \"logLocation\": \"/usr/local/tomcat/logs/CofaxTools.log\",
        \"logMaxSize\": \"\",
        \"dataLog\": 1,
        \"dataLogLocation\": \"/usr/local/tomcat/logs/dataLog.log\",
        \"dataLogMaxSize\": \"\",
        \"removePageCache\": \"/content/admin/remove?cache=pages&id=\",
        \"removeTemplateCache\": \"/content/admin/remove?cache=templates&id=\",
        \"fileTransferFolder\": \"/usr/local/tomcat/webapps/content/fileTransferFolder\",
        \"lookInContext\": 1,
        \"adminGroupID\": 4,
        \"betaServer\": true}}],
  \"servlet-mapping\": {
    \"cofaxCDS\": \"/\",
    \"cofaxEmail\": \"/cofaxutil/aemail/*\",
    \"cofaxAdmin\": \"/admin/*\",
    \"fileServlet\": \"/static/*\",
    \"cofaxTools\": \"/tools/*\"},
 
  \"taglib\": {
    \"taglib-uri\": \"cofax.tld\",
    \"taglib-location\": \"/WEB-INF/tlds/cofax.tld\"}}}"

@timeit (for n in 1:10; a = parse_json(_json_data); end) "json      "
