import { defineConfig } from 'vitepress'
import { tabsMarkdownPlugin } from 'vitepress-plugin-tabs'
// import mathjax3 from "markdown-it-mathjax3";
// import footnote from "markdown-it-footnote";

// https://vitepress.dev/reference/site-config
export default defineConfig({
  base: '/docs.julialang.org.git/',// TODO: replace this in makedocs!
  title: 'The Julia Language',
  description: "A VitePress Site",
  lastUpdated: true,
  cleanUrls: true,
  outDir: '../final_site', // This is required for MarkdownVitepress to work correctly...
  
  ignoreDeadLinks: true,

  markdown: {
    math: false,
    config(md) {
      md.use(tabsMarkdownPlugin)
      // md.use(mathjax3),
      // md.use(footnote)
    },
    theme: {
      light: "github-light",
      dark: "github-dark"}
  },
  themeConfig: {
    outline: 'deep',
    
    search: {
      provider: 'local',
      options: {
        detailedView: true
      }
    },
    nav: [
{ text: 'Julia Documentation', link: '/index' }
],
    sidebar: 'REPLACE_ME_DOCUMENTER_VITEPRESS',
    
    editLink: { pattern: "https://github.com/JuliaLang/docs.julialang.org.git/edit/master/docs/src/:path" },
    socialLinks: [
      { icon: 'github', link: 'https://github.com/JuliaLang/docs.julialang.org.git' }
    ],
    footer: {
      message: 'Made with <a href="https://luxdl.github.io/DocumenterVitepress.jl/dev/" target="_blank"><strong>DocumenterVitepress.jl</strong></a><br>',
      copyright: `© Copyright ${new Date().getUTCFullYear()}.`
    }
  }
})
