document.addEventListener("DOMContentLoaded", () => {
  const toggleButtons = document.querySelectorAll(".bibtex-toggle");

  const copyText = async (text) => {
    try {
      await navigator.clipboard.writeText(text);
      return;
    } catch (error) {
      const fallback = document.createElement("textarea");
      fallback.value = text;
      fallback.style.position = "fixed";
      fallback.style.opacity = "0";
      document.body.appendChild(fallback);
      fallback.focus();
      fallback.select();
      document.execCommand("copy");
      document.body.removeChild(fallback);
    }
  };

  toggleButtons.forEach((button) => {
    button.addEventListener("click", () => {
      const targetId = button.dataset.bibtexTarget;
      const panel = targetId ? document.getElementById(targetId) : null;
      if (!panel) return;

      panel.classList.toggle("d-none");
      button.textContent = panel.classList.contains("d-none") ? "BibTeX" : "Hide BibTeX";
    });
  });

  const copyButtons = document.querySelectorAll(".bibtex-copy-btn");
  copyButtons.forEach((button) => {
    button.addEventListener("click", async () => {
      const targetId = button.dataset.copyTarget;
      const target = targetId ? document.getElementById(targetId) : null;
      if (!target) return;

      await copyText(target.value || target.textContent || "");

      const originalLabel = button.innerHTML;
      button.innerHTML = '<i class="fa-solid fa-clipboard-check"></i> Copied';
      setTimeout(() => {
        button.innerHTML = originalLabel;
      }, 1500);
    });
  });
});
