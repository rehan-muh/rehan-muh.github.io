document.addEventListener("DOMContentLoaded", () => {
  const modalElement = document.getElementById("citationModal");
  const modalContent = document.getElementById("citation-modal-content");
  const copyButton = document.getElementById("citation-modal-copy-btn");
  const triggerButtons = document.querySelectorAll(".citation-modal-trigger");

  if (!modalElement || !modalContent || !copyButton || !triggerButtons.length) return;

  let activeCitation = "";

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

  triggerButtons.forEach((button) => {
    button.addEventListener("click", () => {
      const citationTarget = button.dataset.citationTarget;
      const citationNode = citationTarget ? document.getElementById(citationTarget) : null;
      activeCitation = citationNode ? citationNode.value || citationNode.textContent : "";
      modalContent.textContent = activeCitation;
      window.jQuery(modalElement).modal("show");
    });
  });

  copyButton.addEventListener("click", async () => {
    if (!activeCitation) return;

    await copyText(activeCitation);

    const originalLabel = copyButton.innerHTML;
    copyButton.innerHTML = '<i class="fa-solid fa-clipboard-check"></i> Copied';
    setTimeout(() => {
      copyButton.innerHTML = originalLabel;
    }, 1500);
  });
});
