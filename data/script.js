const hasFailure = document.querySelector(".fail") !== null;

[...document.getElementsByClassName("group")].forEach(group => {
  if (hasFailure && !group.classList.contains("fail")) {
    group.classList.add("rollup");
  }

  group.addEventListener("click", ev => {
    group.classList.toggle("rollup");
  });
  // prevent annoying highlighting when toggling group
  group.addEventListener("mousedown", ev => {
    if (event.detail > 1) { event.preventDefault(); }
  }, false);
})
