(function () {
  "use strict";
  function announce(text) {
    ["copy_status", "export_copy_status"].forEach(function (id) {
      var target = document.getElementById(id);
      if (target) target.textContent = text;
    });
  }
  async function copyMarkdown() {
    var source = document.getElementById("markdown_text");
    if (!source || !source.value.trim()) {
      announce("Enter valid inputs before copying.");
      return;
    }
    var copied = false;
    if (navigator.clipboard && window.isSecureContext) {
      try { await navigator.clipboard.writeText(source.value); copied = true; } catch (_) {}
    }
    if (!copied) {
      var temporary = document.createElement("textarea");
      temporary.value = source.value;
      temporary.style.position = "fixed";
      temporary.style.left = "-9999px";
      document.body.appendChild(temporary);
      temporary.select();
      try { copied = document.execCommand("copy"); } catch (_) {}
      temporary.remove();
    }
    if (copied) announce("Markdown copied.");
    else {
      var tab = document.querySelector('a[data-value="markdown"]');
      if (tab) tab.click();
      source.focus();
      source.select();
      announce("Automatic copying is unavailable. The Markdown is selected; press Ctrl+C or Command+C.");
    }
  }
  document.addEventListener("click", function (event) {
    if (event.target.closest("#copy_markdown, [data-copy-markdown]")) copyMarkdown();
  });
  document.addEventListener("shiny:value", function (event) {
    if (event.name === "markdown_source") announce("");
  });
}());
