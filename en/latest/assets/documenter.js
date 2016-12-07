/*
 * Part of Documenter.jl
 *     https://github.com/JuliaDocs/Documenter.jl
 *
 * License: MIT
 */

requirejs.config({
    paths: {
        'jquery': 'https://code.jquery.com/jquery-3.1.0.js?',
        'jqueryui': 'https://cdnjs.cloudflare.com/ajax/libs/jqueryui/1.12.0/jquery-ui.min',
        'mathjax': 'https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS_HTML',
        'highlight': 'https://cdnjs.cloudflare.com/ajax/libs/highlight.js/9.5.0/highlight.min',
        'highlight-julia': 'https://cdnjs.cloudflare.com/ajax/libs/highlight.js/9.5.0/languages/julia.min',
    },
    shim: {
        'mathjax' : {
            exports: "MathJax"
        },
        'highlight-julia': ['highlight']
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

require(['jquery', 'highlight', 'highlight-julia'], function($, hljs) {
    $(document).ready(function() {
        if (typeof DOC_VERSIONS !== 'undefined') {
            var version_selector = $("#version-selector");
            DOC_VERSIONS.forEach(function(each) {
                var option = $("<option value='" + documenterBaseURL + "/../" + each + "'>" + each + "</option>");
                version_selector.append(option);
            });
        }
        hljs.initHighlighting();
    })

})
