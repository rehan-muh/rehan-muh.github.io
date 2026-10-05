gitbook.require(["jQuery"],function(e){
/*
 * jQuery Highlight plugin
 *
 * Based on highlight v3 by Johann Burkard
 * http://johannburkard.de/blog/programming/javascript/highlight-javascript-text-higlighting-jquery-plugin.html
 *
 * Code a little bit refactored and cleaned (in my humble opinion).
 * Most important changes:
 *  - has an option to highlight only entire words (wordsOnly - false by default),
 *  - has an option to be case sensitive (caseSensitive - false by default)
 *  - highlight element tag and class names can be specified in options
 *
 * Copyright (c) 2009 Bartek Szopka
 *
 * Licensed under MIT license.
 *
 */
e.extend({highlight:function(t,i,n,a){if(3===t.nodeType){var h=t.data.match(i);if(h){var r=document.createElement(n||"span");r.className=a||"highlight";var s=t.splitText(h.index);s.splitText(h[0].length);var l=s.cloneNode(!0);return r.appendChild(l),s.parentNode.replaceChild(r,s),1}}else if(1===t.nodeType&&t.childNodes&&!/(script|style)/i.test(t.tagName)&&(t.tagName!==n.toUpperCase()||t.className!==a))for(var c=0;c<t.childNodes.length;c++)c+=e.highlight(t.childNodes[c],i,n,a);return 0}}),e.fn.unhighlight=function(t){var i={className:"highlight",element:"span"};return e.extend(i,t),this.find(i.element+"."+i.className).each(function(){var e=this.parentNode;e.replaceChild(this.firstChild,this),e.normalize()}).end()},e.fn.highlight=function(t,i){var n={className:"highlight",element:"span",caseSensitive:!1,wordsOnly:!1};if(e.extend(n,i),t.constructor===String&&(t=[t],/\s/.test(t[0])&&t.push(t[0].replace(/\s+/,"-"))),t=e.grep(t,function(e){return""!==e}),0===(t=e.map(t,function(e){return e.replace(/[-[\]{}()*+?.,\\^$|#\s]/g,"\\$&")})).length)return this;var a=n.caseSensitive?"":"i",h="("+t.join("|")+")";n.wordsOnly&&(h="\\b"+h+"\\b");var r=new RegExp(h,a);return this.each(function(){e.highlight(this,r,n.element,n.className)})}});