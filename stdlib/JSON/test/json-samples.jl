#Examples from http://json.org/example.html
a="{\"menu\": {
         \"id\": \"file\",
         \"value\": \"File\",
         \"popup\": {
           \"menuitem\": [
             {\"value\": \"New\", \"onclick\": \"CreateNewDoc()\"},
             {\"value\": \"Open\", \"onclick\": \"OpenDoc()\"},
             {\"value\": \"Close\", \"onclick\": \"CloseDoc()\"}
           ]
         }
       }}
       "


b="{
    \"glossary\": {
        \"title\": \"example glossary\",
    \"GlossDiv\": {
            \"title\": \"S\",
      \"GlossList\": {
                \"GlossEntry\": {
                    \"ID\": \"SGML\",
          \"SortAs\": \"SGML\",
          \"GlossTerm\": \"Standard Generalized Markup Language\",
          \"Acronym\": \"SGML\",
          \"Abbrev\": \"ISO 8879:1986\",
          \"GlossDef\": {
                        \"para\": \"A meta-markup language, used to create markup languages such as DocBook.\",
            \"GlossSeeAlso\": [\"GML\", \"XML\"]
                    },
          \"GlossSee\": \"markup\"
                }
            }
        }
    }
}
"

const c = """
{"widget": {
    "debug": "on",
    "window": {
        "title": "Sample Konfabulator Widget",
        "name": "main_window",
        "width": 500,
        "height": 500
    },
    "image": {
        "src": "Images/Sun.png",
        "name": "sun1",
        "hOffset": 250,
        "vOffset": 250,
        "alignment": "center"
    },
    "text": {
        "data": "Click Here",
        "size": 36.5,
        "style": "bold",
        "name": "text1",
        "hOffset": 250,
        "vOffset": 100,
        "alignment": "center",
        "onMouseUp": "sun1.opacity = (sun1.opacity / 100) * 90;"
    }
}}"""
function validate_c(c)
    j = JSON.parse(c)
    @test j != nothing
    @test typeof(j["widget"]["image"]["hOffset"]) == Int64
    @test j["widget"]["image"]["hOffset"] == 250
    @test typeof(j["widget"]["text"]["size"]) == Float64
    @test j["widget"]["text"]["size"] == 36.5
end

d = "{\"web-app\": {
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

const svg_tviewer_menu = """
{"menu": {
    "header": "SVG\\tViewer\\u03b1",
    "items": [
        {"id": "Open"},
        {"id": "OpenNew", "label": "Open New"},
        null,
        {"id": "ZoomIn", "label": "Zoom In"},
        {"id": "ZoomOut", "label": "Zoom Out"},
        {"id": "OriginalView", "label": "Original View"},
        null,
        {"id": "Quality"},
        {"id": "Pause"},
        {"id": "Mute"},
        null,
        {"id": "Find", "label": "Find..."},
        {"id": "FindAgain", "label": "Find Again"},
        {"id": "Copy"},
        {"id": "CopyAgain", "label": "Copy Again"},
        {"id": "CopySVG", "label": "Copy SVG"},
        {"id": "ViewSVG", "label": "View SVG"},
        {"id": "ViewSource", "label": "View Source"},
        {"id": "SaveAs", "label": "Save As"},
        null,
        {"id": "Help"},
        {"id": "About", "label": "About Adobe SVG Viewer..."}
    ]
}}"""
function validate_svg_tviewer_menu(str)
    j = JSON.parse(str)
    @test j != nothing
    @test typeof(j) == Dict{String, Any}
    @test length(j) == 1
    @test typeof(j["menu"]) == Dict{String, Any}
    @test length(j["menu"]) == 2
    @test j["menu"]["header"] == "SVG\tViewerα"
    @test isa(j["menu"]["items"], Vector{Any})
    @test length(j["menu"]["items"]) == 22
    @test j["menu"]["items"][3] == nothing
    @test j["menu"]["items"][2]["id"] == "OpenNew"
    @test j["menu"]["items"][2]["label"] == "Open New"
end


#Example JSON strings from http://www.jquery4u.com/json/10-example-json-files/

gmaps= "{\"markers\": [
        {
            \"point\":\"new GLatLng(40.266044,-74.718479)\",
            \"homeTeam\":\"Lawrence Library\",
            \"awayTeam\":\"LUGip\",
            \"markerImage\":\"images/red.png\",
            \"information\": \"Linux users group meets second Wednesday of each month.\",
            \"fixture\":\"Wednesday 7pm\",
            \"capacity\":\"\",
            \"previousScore\":\"\"
        },
        {
            \"point\":\"new GLatLng(40.211600,-74.695702)\",
            \"homeTeam\":\"Hamilton Library\",
            \"awayTeam\":\"LUGip HW SIG\",
            \"markerImage\":\"images/white.png\",
            \"information\": \"Linux users can meet the first Tuesday of the month to work out harward and configuration issues.\",
            \"fixture\":\"Tuesday 7pm\",
            \"capacity\":\"\",
            \"tv\":\"\"
        },
        {
            \"point\":\"new GLatLng(40.294535,-74.682012)\",
            \"homeTeam\":\"Applebees\",
            \"awayTeam\":\"After LUPip Mtg Spot\",
            \"markerImage\":\"images/newcastle.png\",
            \"information\": \"Some of us go there after the main LUGip meeting, drink brews, and talk.\",
            \"fixture\":\"Wednesday whenever\",
            \"capacity\":\"2 to 4 pints\",
            \"tv\":\"\"
        }
] }"

colors1 = "{
    \"colorsArray\":[{
            \"colorName\":\"red\",
            \"hexValue\":\"#f00\"
        },
        {
            \"colorName\":\"green\",
            \"hexValue\":\"#0f0\"
        },
        {
            \"colorName\":\"blue\",
            \"hexValue\":\"#00f\"
        },
        {
            \"colorName\":\"cyan\",
            \"hexValue\":\"#0ff\"
        },
        {
            \"colorName\":\"magenta\",
            \"hexValue\":\"#f0f\"
        },
        {
            \"colorName\":\"yellow\",
            \"hexValue\":\"#ff0\"
        },
        {
            \"colorName\":\"black\",
            \"hexValue\":\"#000\"
        }
    ]
}"

colors2 = "{
    \"colorsArray\":[{
            \"red\":\"#f00\",
            \"green\":\"#0f0\",
            \"blue\":\"#00f\",
            \"cyan\":\"#0ff\",
            \"magenta\":\"#f0f\",
            \"yellow\":\"#ff0\",
            \"black\":\"#000\"
        }
    ]
}"

colors3 = "{
    \"red\":\"#f00\",
    \"green\":\"#0f0\",
    \"blue\":\"#00f\",
    \"cyan\":\"#0ff\",
    \"magenta\":\"#f0f\",
    \"yellow\":\"#ff0\",
    \"black\":\"#000\"
}"

twitter = "{\"results\":[

     {\"text\":\"@twitterapi  http://tinyurl.com/ctrefg\",
     \"to_user_id\":396524,
     \"to_user\":\"TwitterAPI\",
     \"from_user\":\"jkoum\",
     \"metadata\":
         {
          \"result_type\":\"popular\",
          \"recent_retweets\": 109
         },
     \"id\":1478555574,
     \"from_user_id\":1833773,
     \"iso_language_code\":\"nl\",
     \"source\":\"<a href=\\\"http://twitter.com/\\\">twitter</a>\",
     \"profile_image_url\":\"http://s3.amazonaws.com/twitter_production/profile_images/118412707/2522215727_a5f07da155_b_normal.jpg\",
     \"created_at\":\"Wed, 08 Apr 2009 19:22:10 +0000\"}],
     \"since_id\":0,
     \"max_id\":1480307926,
     \"refresh_url\":\"?since_id=1480307926&q=%40twitterapi\",
     \"results_per_page\":15,
     \"next_page\":\"?page=2&max_id=1480307926&q=%40twitterapi\",
     \"completed_in\":0.031704,
     \"page\":1,
     \"query\":\"%40twitterapi\"}"

facebook= "{
   \"data\": [
      {
         \"id\": \"X999_Y999\",
         \"from\": {
            \"name\": \"Tom Brady\", \"id\": \"X12\"
         },
         \"message\": \"Looking forward to 2010!\",
         \"actions\": [
            {
               \"name\": \"Comment\",
               \"link\": \"http://www.facebook.com/X999/posts/Y999\"
            },
            {
               \"name\": \"Like\",
               \"link\": \"http://www.facebook.com/X999/posts/Y999\"
            }
         ],
         \"type\": \"status\",
         \"created_time\": \"2010-08-02T21:27:44+0000\",
         \"updated_time\": \"2010-08-02T21:27:44+0000\"
      },
      {
         \"id\": \"X998_Y998\",
         \"from\": {
            \"name\": \"Peyton Manning\", \"id\": \"X18\"
         },
         \"message\": \"Where's my contract?\",
         \"actions\": [
            {
               \"name\": \"Comment\",
               \"link\": \"http://www.facebook.com/X998/posts/Y998\"
            },
            {
               \"name\": \"Like\",
               \"link\": \"http://www.facebook.com/X998/posts/Y998\"
            }
         ],
         \"type\": \"status\",
         \"created_time\": \"2010-08-02T21:27:44+0000\",
         \"updated_time\": \"2010-08-02T21:27:44+0000\"
      }
   ]
}"

const flickr = """{
    "title": "Talk On Travel Pool",
    "link": "http://www.flickr.com/groups/talkontravel/pool/",
    "description": "Travel and vacation photos from around the world.",
    "modified": "2009-02-02T11:10:27Z",
    "generator": "http://www.flickr.com/",
    "totalItems":222,
    "items": [
            {
            "title": "View from the hotel",
            "link": "http://www.flickr.com/photos/33112458@N08/3081564649/in/pool-998875@N22",
            "media": {"m":"http://farm4.static.flickr.com/3037/3081564649_4a6569750c_m.jpg"},
            "date_taken": "2008-12-04T04:43:03-08:00",
            "description": "<p><a href=\\"http://www.flickr.com/people/33112458@N08/\\"> Talk On Travel</a> has added a photo to the pool:</p> <p><a href=\\"http:// www.flickr.com/photos/33112458@N08/3081564649/\\" title=\\"View from the hotel\\"> <img src=\\"http://farm4.static.flickr.com/3037/3081564649_4a6569750c_m.jpg\\" width=\\"240\\" height=\\"180\\" alt=\\"View from the hotel\\" /></a></p> ",
            "published": "2008-12-04T12:43:03Z",
            "author": "nobody@flickr.com (Talk On Travel)",
            "author_id": "33112458@N08",
            "tags": "spain dolphins tenerife canaries lagomera aqualand playadelasamericas junglepark losgigantos loscristines talkontravel"
            }
    ]
}"""
function validate_flickr(str)
    k = JSON.parse(str)
    @test k != nothing
    @test k["totalItems"] == 222
    @test k["items"][1]["description"][12] == '\"'
end

youtube = "{\"apiVersion\":\"2.0\",
 \"data\":{
    \"updated\":\"2010-01-07T19:58:42.949Z\",
    \"totalItems\":800,
    \"startIndex\":1,
    \"itemsPerPage\":1,
    \"items\":[
        {\"id\":\"hYB0mn5zh2c\",
         \"uploaded\":\"2007-06-05T22:07:03.000Z\",
         \"updated\":\"2010-01-07T13:26:50.000Z\",
         \"uploader\":\"GoogleDeveloperDay\",
         \"category\":\"News\",
         \"title\":\"Google Developers Day US - Maps API Introduction\",
         \"description\":\"Google Maps API Introduction ...\",
         \"tags\":[
            \"GDD07\",\"GDD07US\",\"Maps\"
         ],
         \"thumbnail\":{
            \"default\":\"http://i.ytimg.com/vi/hYB0mn5zh2c/default.jpg\",
            \"hqDefault\":\"http://i.ytimg.com/vi/hYB0mn5zh2c/hqdefault.jpg\"
         },
         \"player\":{
            \"default\":\"http://www.youtube.com/watch?v\u003dhYB0mn5zh2c\"
         },
         \"content\":{
            \"1\":\"rtsp://v5.cache3.c.youtube.com/CiILENy.../0/0/0/video.3gp\",
            \"5\":\"http://www.youtube.com/v/hYB0mn5zh2c?f...\",
            \"6\":\"rtsp://v1.cache1.c.youtube.com/CiILENy.../0/0/0/video.3gp\"
         },
         \"duration\":2840,
         \"aspectRatio\":\"widescreen\",
         \"rating\":4.63,
         \"ratingCount\":68,
         \"viewCount\":220101,
         \"favoriteCount\":201,
         \"commentCount\":22,
         \"status\":{
            \"value\":\"restricted\",
            \"reason\":\"limitedSyndication\"
         },
         \"accessControl\":{
            \"syndicate\":\"allowed\",
            \"commentVote\":\"allowed\",
            \"rate\":\"allowed\",
            \"list\":\"allowed\",
            \"comment\":\"allowed\",
            \"embed\":\"allowed\",
            \"videoRespond\":\"moderated\"
         }
        }
    ]
 }
}"

iphone = "{
    \"menu\": {
        \"header\": \"xProgress SVG Viewer\",
        \"items\": [
            {
                \"id\": \"Open\"
            },
            {
                \"id\": \"OpenNew\",
                \"label\": \"Open New\"
            },
            null,
            {
                \"id\": \"ZoomIn\",
                \"label\": \"Zoom In\"
            },
            {
                \"id\": \"ZoomOut\",
                \"label\": \"Zoom Out\"
            },
            {
                \"id\": \"OriginalView\",
                \"label\": \"Original View\"
            },
            null,
            {
                \"id\": \"Quality\"
            },
            {
                \"id\": \"Pause\"
            },
            {
                \"id\": \"Mute\"
            },
            null,
            {
                \"id\": \"Find\",
                \"label\": \"Find...\"
            },
            {
                \"id\": \"FindAgain\",
                \"label\": \"Find Again\"
            },
            {
                \"id\": \"Copy\"
            },
            {
                \"id\": \"CopyAgain\",
                \"label\": \"Copy Again\"
            },
            {
                \"id\": \"CopySVG\",
                \"label\": \"Copy SVG\"
            },
            {
                \"id\": \"ViewSVG\",
                \"label\": \"View SVG\"
            },
            {
                \"id\": \"ViewSource\",
                \"label\": \"View Source\"
            },
            {
                \"id\": \"SaveAs\",
                \"label\": \"Save As\"
            },
            null,
            {
                \"id\": \"Help\"
            },
            {
                \"id\": \"About\",
                \"label\": \"About xProgress CVG Viewer...\"
            }
        ]
    }
}"

customer = "{
     \"firstName\": \"John\",
     \"lastName\": \"Smith\",
     \"age\": 25,
     \"address\":
     {
         \"streetAddress\": \"21 2nd Street\",
         \"city\": \"New York\",
         \"state\": \"NY\",
         \"postalCode\": \"10021\"
     },
     \"phoneNumber\":
     [
         {
           \"type\": \"home\",
           \"number\": \"212 555-1234\"
         },
         {
           \"type\": \"fax\",
           \"number\": \"646 555-4567\"
         }
    ]
 }"

 product = "{
        \"name\":\"Product\",
        \"properties\":
        {
                \"id\":
                {
                        \"type\":\"number\",
                        \"description\":\"Product identifier\",
                        \"required\":true
                },
                \"name\":
                {
                        \"description\":\"Name of the product\",
                        \"type\":\"string\",
                        \"required\":true
                },
                \"price\":
                {
                        \"type\":\"number\",
                        \"minimum\":0,
                        \"required\":true
                },
                \"tags\":
                {
                        \"type\":\"array\",
                        \"items\":
                        {
                                \"type\":\"string\"
                        }
                }
        }
}"

interop = "{
    \"ResultSet\": {
        \"totalResultsAvailable\": \"1827221\",
        \"totalResultsReturned\": 2,
        \"firstResultPosition\": 1,
        \"Result\": [
            {
                \"Title\": \"potato jpg\",
                \"Summary\": \"Kentang Si bungsu dari keluarga Solanum tuberosum L ini ternyata memiliki khasiat untuk mengurangi kerutan  jerawat  bintik hitam dan kemerahan pada kulit  Gunakan seminggu sekali sebagai\",
                \"Url\": \"http://www.mediaindonesia.com/spaw/uploads/images/potato.jpg\",
                \"ClickUrl\": \"http://www.mediaindonesia.com/spaw/uploads/images/potato.jpg\",
                \"RefererUrl\": \"http://www.mediaindonesia.com/mediaperempuan/index.php?ar_id=Nzkw\",
                \"FileSize\": 22630,
                \"FileFormat\": \"jpeg\",
                \"Height\": \"362\",
                \"Width\": \"532\",
                \"Thumbnail\": {
                    \"Url\": \"http://thm-a01.yimg.com/nimage/557094559c18f16a\",
                    \"Height\": \"98\",
                    \"Width\": \"145\"
                }
            },
            {
                \"Title\": \"potato jpg\",
                \"Summary\": \"Introduction of puneri aloo This is a traditional potato preparation flavoured with curry leaves and peanuts and can be eaten on fasting day  Preparation time   10 min\",
                \"Url\": \"http://www.infovisual.info/01/photo/potato.jpg\",
                \"ClickUrl\": \"http://www.infovisual.info/01/photo/potato.jpg\",
                \"RefererUrl\": \"http://sundayfood.com/puneri-aloo-indian-%20recipe\",
                \"FileSize\": 119398,
                \"FileFormat\": \"jpeg\",
                \"Height\": \"685\",
                \"Width\": \"1024\",
                \"Thumbnail\": {
                    \"Url\": \"http://thm-a01.yimg.com/nimage/7fa23212efe84b64\",
                    \"Height\": \"107\",
                    \"Width\": \"160\"
                }
            }
        ]
    }
}"

const unicode = """
{"অলিম্পিকস": {
    "অ্যাথলেট": "২২টি দেশ থেকে ২,০৩৫ জন প্রতিযোগী",
    "ইভেন্ট": "২২টি ইভেন্টের মধ্যে ছিল দড়ি টানাটানি",
    "রেকর্ড": [
        {"১০০মি. স্প্রিন্ট": "রেজি ওয়াকার, দক্ষিণ আফ্রিকা"},
        {"Marathon": "জনি হেইস"},
        {" ফ্রি-স্টাইল সাঁতার": "Henry Taylor, Britain"}
    ]
}}
"""
function validate_unicode(str)
    u = JSON.parse(str)
    @test u != nothing
    @test u["অলিম্পিকস"]["রেকর্ড"][2]["Marathon"] == "জনি হেইস"
end
