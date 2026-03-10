document.addEventListener("DOMContentLoaded", () => {
  const citationButtons = document.querySelectorAll("[data-copy-citation]");

  citationButtons.forEach((button) => {
    button.addEventListener("click", async () => {
      const citation = button.dataset.copyCitation;
      if (!citation) return;

      try {
        await navigator.clipboard.writeText(citation);
      } catch (error) {
        const fallback = document.createElement("textarea");
        fallback.value = citation;
        fallback.style.position = "fixed";
        fallback.style.opacity = "0";
        document.body.appendChild(fallback);
        fallback.focus();
        fallback.select();
        document.execCommand("copy");
        document.body.removeChild(fallback);
      }

      const originalLabel = button.innerHTML;
      button.innerHTML = '<i class="fa-solid fa-clipboard-check"></i> Copied';
      setTimeout(() => {
        button.innerHTML = originalLabel;
      }, 1500);
    });
  });
});
