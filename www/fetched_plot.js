// Embedding plots downloaded over HTTP with a progress bar, and the loading
// screen shown until the app is connected.
//
// The output "<id>_payload" of a plot (fetched_plot_output(),
// functions/ui/fetched_plot.R) receives a small value from the server:
// where the session serves the JSON of the plot and its size
// (server/plot_payload.R). The JSON is downloaded here, with the bytes
// received shown on a progress bar, and drawn in the plotly widget through
// the htmlwidgets output binding, as if it had arrived over the websocket.
// The view of a plot (camera or axis ranges) that the user has changed is
// kept when the plot is redrawn on the same axes, e.g. in another colour.

(function () {
  const inflight = {};
  const outputPlotly = {};

  function formatMB(bytes) {
    return (bytes / 1e6).toFixed(1) + ' MB';
  }

  // Shows `text` over the plot, with the bar at `fraction` (0..1), moving
  // when null, absent when undefined (a message).
  function showProgress(el, text, fraction) {
    const box = el.querySelector('.plot-progress');
    if (!box) return;
    box.querySelector('.plot-progress-text').textContent = text;
    const fill = box.querySelector('.plot-progress-fill');
    fill.style.width = (fraction ? Math.round(100 * fraction) : 0) + '%';
    box.classList.toggle('plot-progress-indeterminate', fraction === null);
    box.classList.toggle('plot-progress-no-bar', fraction === undefined);
    box.classList.add('plot-progress-shown');
  }

  function hideProgress(el) {
    const box = el.querySelector('.plot-progress');
    if (box) box.classList.remove('plot-progress-shown');
  }

  // The htmlwidgets output binding of the plotly widgets (registered by
  // htmlwidgets under the name of the widget).
  function widgetBinding() {
    const found = Shiny.outputBindings.getBindings().find(function (entry) {
      return entry.binding.name === 'plotly';
    });
    return found ? found.binding : null;
  }

  // Downloads `url`, reporting the bytes received to `onProgress`.
  async function download(url, onProgress, signal) {
    const response = await fetch(url, { signal: signal, cache: 'no-store' });
    if (!response.ok) throw new Error('HTTP ' + response.status);
    const reader = response.body.getReader();
    const chunks = [];
    let received = 0;
    for (;;) {
      const chunk = await reader.read();
      if (chunk.done) break;
      chunks.push(chunk.value);
      received += chunk.value.length;
      onProgress(received);
    }
    const bytes = new Uint8Array(received);
    let offset = 0;
    chunks.forEach(function (chunk) { bytes.set(chunk, offset); offset += chunk.length; });
    return new TextDecoder('utf-8').decode(bytes);
  }

  // --- the view of a plot the user has changed ---------------------------

  const userViews = {};

  function titleText(axis) {
    const title = axis && axis.title;
    return title && typeof title === 'object' ? title.text : title;
  }

  // The axes a plot is drawn on: the same key, the same view applies.
  function viewKey(layout) {
    if (!layout) return null;
    if (layout.scene) {
      return '3d|' + [layout.scene.xaxis, layout.scene.yaxis, layout.scene.zaxis].map(titleText).join('|');
    }
    return '2d|' + [layout.xaxis, layout.yaxis].map(titleText).join('|');
  }

  // Merges a relayout `update` into the saved `view`: a new setting of an
  // axis or of the camera replaces the old one.
  function mergeView(view, update) {
    const merged = Object.assign({}, view);
    Object.keys(update).forEach(function (key) {
      const parts = key.split('.');
      const prefix = parts[0] === 'scene' ? parts.slice(0, 2).join('.') : parts[0];
      Object.keys(merged).forEach(function (old) {
        if (old === prefix || old.indexOf(prefix + '.') === 0 || old.indexOf(prefix + '[') === 0) delete merged[old];
      });
      merged[key] = update[key];
    });
    return merged;
  }

  $(document).on('plotly_relayout', '.fetched-plot .js-plotly-plot', function (event, data) {
    if (!data || !this.layout) return;
    const update = linkedViewUpdate(data);
    if (Object.keys(update).length === 0) return;
    const key = viewKey(this.layout);
    const saved = userViews[this.id];
    userViews[this.id] = { key: key, view: mergeView(saved && saved.key === key ? saved.view : {}, update) };
  });

  function restoreView(graphDiv, layout) {
    const saved = userViews[graphDiv.id];
    if (!saved || saved.key !== viewKey(layout)) return;
    $(graphDiv).one('plotly_afterplot', function () {
      Plotly.relayout(graphDiv, saved.view);
    });
  }

  // --- the output binding ------------------------------------------------

  const binding = new Shiny.OutputBinding();

  binding.find = function (scope) {
    return $(scope).find('.fetched-plot');
  };

  binding.renderValue = function (el, data) {
    if (inflight[el.id]) inflight[el.id].abort();
    const graphDiv = el.querySelector('.html-widget-output');
    if (!graphDiv) return;
    el.classList.remove('downloading');
    if (!data || data.message) {
      // nothing to draw: the previous plot goes, a message may take its place
      if (graphDiv.data) Plotly.purge(graphDiv);
      if (data && data.message) showProgress(el, data.message); else hideProgress(el);
      return;
    }
    const controller = new AbortController();
    inflight[el.id] = controller;
    const total = data.bytes || 0;
    const started = Date.now();
    el.classList.add('downloading');
    showProgress(el, 'Downloading the plot: 0.0 of ' + formatMB(total), 0);

    const text = data.json !== undefined ? Promise.resolve(data.json) : download(data.url, function (received) {
      const seconds = (Date.now() - started) / 1000;
      const speed = seconds > 0.5 ? ', ' + formatMB(received / seconds) + '/s' : '';
      showProgress(el, 'Downloading the plot: ' + formatMB(received) + ' of ' + formatMB(total) + speed,
                   total > 0 ? Math.min(1, received / total) : null);
    }, controller.signal);

    text.then(function (json) {
      const payload = JSON.parse(json);
      const widget = widgetBinding();
      if (!widget) throw new Error('no plot widget');
      restoreView(graphDiv, payload.x && payload.x.layout);
      widget.renderValue(graphDiv, payload);
      // Shiny marks the widget, an output it never computes, as recalculating
      graphDiv.classList.remove('recalculating');
      hideProgress(el);
      el.classList.remove('downloading');
      delete inflight[el.id];
    }).catch(function (error) {
      if (error.name === 'AbortError') return;
      el.classList.remove('downloading');
      showProgress(el, 'The plot could not be loaded (' + error.message + ')');
    });
  };

  // Shiny's own error handler empties the output element, which would
  // remove the widget: a silent error (req() not met: nothing to draw) just
  // takes the progress bar away, another error is shown in its place.
  binding.renderError = function (el, err) {
    if (inflight[el.id]) inflight[el.id].abort();
    el.classList.remove('downloading');
    if (err && err.message) {
      showProgress(el, 'The plot could not be drawn: ' + err.message);
    } else {
      hideProgress(el);
    }
  };

  binding.clearError = function (el) {
    hideProgress(el);
  };

  Shiny.outputBindings.register(binding, 'plavisca.fetchedPlot');

  // while the server prepares a plot
  $(document).on('shiny:recalculating', '.fetched-plot', function (event) {
    showProgress(event.target, 'Preparing the plot', null);
  });

  // --- the loading screen ------------------------------------------------

  function hideLoadingScreen() {
    const screen = document.getElementById('app-loading');
    if (screen) screen.remove();
  }
  $(document).on('shiny:sessioninitialized shiny:disconnected', hideLoadingScreen);
})();
