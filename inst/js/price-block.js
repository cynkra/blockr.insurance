/**
 * Price block — Shiny input binding for Blockr.Select.single
 *
 * Renders a div with `data-blockr-price-select` as a Blockr.Select dropdown,
 * and pipes value changes back to Shiny as input values. Reuses the same
 * select widget blockr.dplyr blocks use (Blockr.Select), so the engine
 * version dropdown matches the look-and-feel of the rest of the dock.
 *
 * Markup contract (rendered server-side by R):
 *   <div data-blockr-price-select id="..."
 *        data-options='[{"value":"engine_property","label":""}, ...]'
 *        data-selected="engine_property"></div>
 */
(() => {
  'use strict';

  const binding = new Shiny.InputBinding();

  $.extend(binding, {
    find(scope) {
      return $(scope).find('[data-blockr-price-select]');
    },

    initialize(el) {
      const optsRaw = el.getAttribute('data-options') || '[]';
      const selected = el.getAttribute('data-selected') || '';
      let options;
      try {
        options = JSON.parse(optsRaw);
      } catch (e) {
        console.error('price-block: bad data-options', optsRaw, e);
        options = [];
      }

      // Wrap with .blockr-row + label so it matches the dplyr-style layout.
      // The container itself becomes the Blockr.Select host.
      const select = Blockr.Select.single(el, {
        options,
        selected,
        onChange: (val) => {
          el._blockrValue = val;
          $(el).trigger('change');
        }
      });
      // Bordered modifier — standalone select gets the framed look that
      // matches blockr.dplyr's bordered selects.
      select.el.classList.add('blockr-select--bordered');

      el._blockrValue = selected;
      el._blockrSelect = select;
    },

    getValue(el) {
      return el._blockrValue;
    },

    setValue(el, value) {
      if (el._blockrSelect) {
        el._blockrSelect.setValue?.(value);
        el._blockrValue = value;
      }
    },

    subscribe(el, callback) {
      $(el).on('change.blockrPriceSelect', () => callback());
    },

    unsubscribe(el) {
      $(el).off('.blockrPriceSelect');
    },

    receiveMessage(el, data) {
      if ('options' in data && el._blockrSelect?.setOptions) {
        el._blockrSelect.setOptions(data.options);
      }
      if ('selected' in data) {
        binding.setValue(el, data.selected);
      }
    }
  });

  Shiny.inputBindings.register(binding, 'blockr.insurance.price-select');
})();
