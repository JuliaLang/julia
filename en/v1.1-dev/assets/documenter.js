/*
 * Part of Documenter.jl
 *     https://github.com/JuliaDocs/Documenter.jl
 *
 * License: MIT
 */

requirejs.config({
    paths: {
        'jquery': 'https://cdnjs.cloudflare.com/ajax/libs/jquery/3.1.1/jquery.min',
        'jqueryui': 'https://cdnjs.cloudflare.com/ajax/libs/jqueryui/1.12.0/jquery-ui.min',
        'headroom': 'https://cdnjs.cloudflare.com/ajax/libs/headroom/0.9.3/headroom.min',
        'mathjax': 'https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.1/MathJax.js?config=TeX-AMS_HTML',
        'highlight': 'https://cdnjs.cloudflare.com/ajax/libs/highlight.js/9.12.0/highlight.min',
        'highlight-julia': 'https://cdnjs.cloudflare.com/ajax/libs/highlight.js/9.12.0/languages/julia.min',
        'highlight-julia-repl': 'https://cdnjs.cloudflare.com/ajax/libs/highlight.js/9.12.0/languages/julia-repl.min',
    },
    shim: {
        'mathjax' : {
            exports: "MathJax"
        },
        'highlight-julia': ['highlight'],
        'highlight-julia-repl': ['highlight'],
    }
});

// Load MathJax
require(['mathjax'], function(MathJax) {
    MathJax.Hub.Config({
      "tex2jax": {
        inlineMath: [['$','$'], ['\\(','\\)']],
        processEscapes: true
      }
    });
    MathJax.Hub.Config({
      config: ["MMLorHTML.js"],
      jax: [
        "input/TeX",
        "output/HTML-CSS",
        "output/NativeMML"
      ],
      extensions: [
        "MathMenu.js",
        "MathZoom.js",
        "TeX/AMSmath.js",
        "TeX/AMSsymbols.js",
        "TeX/autobold.js",
        "TeX/autoload-all.js"
      ]
    });
    MathJax.Hub.Config({
      TeX: { equationNumbers: { autoNumber: "AMS" } }
    });
})

require(['jquery', 'highlight', 'highlight-julia', 'highlight-julia-repl'], function($, hljs) {
    $(document).ready(function() {
        hljs.initHighlighting();
    })

})

// update the version selector with info from the siteinfo.js and ../versions.js files
require(['jquery'], function($) {
    $(document).ready(function() {
        var version_selector = $("#version-selector");

        // add the current version to the selector based on siteinfo.js, but only if the selector is empty
        if (typeof DOCUMENTER_CURRENT_VERSION !== 'undefined' && $('#version-selector > option').length == 0) {
            var option = $("<option value='#' selected='selected'>" + DOCUMENTER_CURRENT_VERSION + "</option>");
            version_selector.append(option);
        }

        if (typeof DOC_VERSIONS !== 'undefined') {
            var existing_versions = $('#version-selector > option');
            var existing_versions_texts = existing_versions.map(function(i,x){return x.text});
            DOC_VERSIONS.forEach(function(each) {
                var version_url = documenterBaseURL + "/../" + each;
                var existing_id = $.inArray(each, existing_versions_texts);
                // if not already in the version selector, add it as a new option,
                // otherwise update the old option with the URL and enable it
                if (existing_id == -1) {
                    var option = $("<option value='" + version_url + "'>" + each + "</option>");
                    version_selector.append(option);
                } else {
                    var option = existing_versions[existing_id];
                    option.value = version_url;
                    option.disabled = false;
                }
            });
        }

        // only show the version selector if the selector has been populated
        if ($('#version-selector > option').length > 0) {
            version_selector.css("visibility", "visible");
        }
    })

})

// mobile
require(['jquery', 'headroom'], function($, Headroom) {
    $(document).ready(function() {
        var navtoc = $("nav.toc");
        $("nav.toc li.current a.toctext").click(function() {
            navtoc.toggleClass('show');
        });
        $("article > header div#topbar a.fa-bars").click(function(ev) {
            ev.preventDefault();
            navtoc.toggleClass('show');
            if (navtoc.hasClass('show')) {
                var title = $("article > header div#topbar span").text();
                $("nav.toc ul li a:contains('" + title + "')").focus();
            }
        });
        $("article#docs").bind('click', function(ev) {
            if ($(ev.target).is('div#topbar a.fa-bars')) {
                return;
            }
            if (navtoc.hasClass('show')) {
                navtoc.removeClass('show');
            }
        });
        if ($("article > header div#topbar").css('display') == 'block') {
            var headroom = new Headroom(document.querySelector("article > header div#topbar"), {"tolerance": {"up": 10, "down": 10}});
            headroom.init();
        }
    })
})
