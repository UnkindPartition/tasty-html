[...document.getElementsByClassName("group")].forEach(group => {
  group.addEventListener("click", ev => {
    group.classList.toggle("rollup");
  });
  // prevent annoying highlighting when toggling group
  group.addEventListener('mousedown', ev => {
    if (event.detail > 1) { event.preventDefault(); }
  }, false);
})
