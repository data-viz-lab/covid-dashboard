const resizeObserver = new ResizeObserver((entries) => {
  for (let entry of entries) {
    if (entry.contentBoxSize) {
      // Checking for chrome as using a non-standard array
      if (entry.contentBoxSize[0]) {
        app.ports.updateDimensions.send(entry.contentBoxSize[0]);
      } else {
        app.ports.updateDimensions.send(entry.contentBoxSize);
      }
    } else {
      app.ports.updateDimensions.send(entry.contentRect);
    }
  }
});

app.ports.observeDimensions.subscribe((classSelector) => {
  const callback = () => {
    const el = document.querySelector(`.${classSelector}`);

    if (el) {
      resizeObserver.observe(el);
      clearInterval(intervalID);
    }
  };
  const intervalID = window.setInterval(callback, 200);
});
