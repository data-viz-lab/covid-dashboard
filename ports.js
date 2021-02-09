let counter = 0;

const resizeObserver = new ResizeObserver((entries) => {
  for (let entry of entries) {
    const updateDimensions = app.ports.updateDimensions;
    const windowDimensions = { windowWidth: window.innerWidth, windowHeight: window.innerHeight };

    if (counter < 15) {
      counter += 1;
      if (entry.contentBoxSize) {
        // Checking for chrome as using a non-standard array
        if (entry.contentBoxSize[0]) {
          updateDimensions.send({
            ...windowDimensions,
            height: entry.contentBoxSize[0].inlineSize,
            width: entry.contentBoxSize[0].blockSize,
          });
        } else {
          updateDimensions.send({
            ...windowDimensions,
            height: entry.contentBoxSize.inlineSize,
            width: entry.contentBoxSize.blockSize,
          });
        }
      } else {
        updateDimensions.send({ ...windowDimensions, ...entry.contentRect });
      }
    }
  }
});

app.ports.observeDimensions.subscribe((classSelector) => {
  const callback = () => {
    const el = document.querySelector(classSelector);

    if (el) {
      resizeObserver.observe(el);
      clearInterval(intervalID);
    }
  };
  const intervalID = window.setInterval(callback, 200);
});

app.ports.storeResponseData.subscribe(function (data) {
  if (data.length > 0) {
    localStorage.setItem("covid-data", data);
  }
});
