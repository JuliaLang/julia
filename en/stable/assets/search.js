/*
 * Part of Documenter.jl
 *     https://github.com/JuliaDocs/Documenter.jl
 *
 * License: MIT
 */

// parseUri 1.2.2
// (c) Steven Levithan <stevenlevithan.com>
// MIT License
function parseUri (str) {
	var	o   = parseUri.options,
		m   = o.parser[o.strictMode ? "strict" : "loose"].exec(str),
		uri = {},
		i   = 14;

	while (i--) uri[o.key[i]] = m[i] || "";

	uri[o.q.name] = {};
	uri[o.key[12]].replace(o.q.parser, function ($0, $1, $2) {
		if ($1) uri[o.q.name][$1] = $2;
	});

	return uri;
};
parseUri.options = {
	strictMode: false,
	key: ["source","protocol","authority","userInfo","user","password","host","port","relative","path","directory","file","query","anchor"],
	q:   {
		name:   "queryKey",
		parser: /(?:^|&)([^&=]*)=?([^&]*)/g
	},
	parser: {
		strict: /^(?:([^:\/?#]+):)?(?:\/\/((?:(([^:@]*)(?::([^:@]*))?)?@)?([^:\/?#]*)(?::(\d*))?))?((((?:[^?#\/]*\/)*)([^?#]*))(?:\?([^#]*))?(?:#(.*))?)/,
		loose:  /^(?:(?![^:@]+:[^:@\/]*@)([^:\/?#.]+):)?(?:\/\/)?((?:(([^:@]*)(?::([^:@]*))?)?@)?([^:\/?#]*)(?::(\d*))?)(((\/(?:[^?#](?![^?#\/]*\.[^?#\/.]+(?:[?#]|$)))*\/?)?([^?#\/]*))(?:\?([^#]*))?(?:#(.*))?)/
	}
};

requirejs.config({
    paths: {
        'jquery': 'https://code.jquery.com/jquery-3.1.0.js?',
        'lunr': 'https://cdnjs.cloudflare.com/ajax/libs/lunr.js/0.7.1/lunr.min',
    }
});

var currentScript = document.currentScript;

require(["jquery", "lunr"], function($, lunr) {
    var index = lunr(function () {
        this.ref('location')
        this.field('title', {boost: 10})
        this.field('text')
    })
    var store = {}

    documenterSearchIndex['docs'].forEach(function(e) {
        index.add(e)
        store[e.location] = e
    })

    $(function(){
        function update_search(query) {
            results = index.search(query)
            $('#search-info').text("Number of results: " + results.length)
            $('#search-results').empty()
            results.forEach(function(result) {
                data = store[result.ref]
                link = $('<a>')
                link.text(data.title)
                link.attr('href', documenterBaseURL+'/'+result.ref)
                cat = $('<span class="category">('+data.category+')</span>')
                li = $('<li>').append(link).append(cat)
                $('#search-results').append(li)
            })
        }

        function update_search_box() {
            query = $('#search-query').val()
            update_search(query)
        }

        $('#search-query').keyup(update_search_box)
        $('#search-query').change(update_search_box)

        search_query = parseUri(window.location).queryKey["q"]
        if(search_query !== undefined) {
            $("#search-query").val(search_query)
        }
        update_search_box();
    })
})
