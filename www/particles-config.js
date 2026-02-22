// particles-config.js

function initParticles(targetId) {
  if (typeof particlesJS !== 'undefined') {
    const config = {
      particles: {
        number: { value: 100, density: { enable: true, value_area: 800 } },
        color: { value: '#008080' },
        shape: { type: 'circle', stroke: { width: 0, color: '#000000' } },
        opacity: { value: 0.5 },
        size: { value: 3, random: true },
        line_linked: {
          enable: true, distance: 150, color: '#008080', opacity: 0.3, width: 1
        },
        move: {
          enable: true, speed: 1.5, direction: 'none', out_mode: 'out'
        }
      },
      interactivity: {
        detect_on: 'canvas',
        events: {
          onhover: { enable: true, mode: 'grab' },
          onclick: { enable: true, mode: 'push' }
        },
        modes: {
          grab: { distance: 140, line_linked: { opacity: 0.8 } },
          push: { particles_nb: 4 }
        }
      },
      retina_detect: true
    };

    // Destroy only existing particles for the given targetId
    if (window.pJSDom && window.pJSDom.length) {
      const existingInstance = window.pJSDom.find(p =>
        p.pJS && p.pJS.canvas && p.pJS.canvas.el &&
        p.pJS.canvas.el.parentElement &&
        p.pJS.canvas.el.parentElement.id === targetId
      );

      if (existingInstance) {
        // existingInstance.pJS.fn.vendors.destroypJS();
        window.pJSDom = window.pJSDom.filter(p => p !== existingInstance);
      }
    }

    // Create new particles instance for the given target
    particlesJS(targetId, config);
  } else {
    console.error("particlesJS not loaded.");
  }
}


$(document).ready(function () {
  initParticles('particles-js-home');
  initParticles('particles-js-about');
});
