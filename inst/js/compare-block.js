// @ts-check
/**
 * CompareBlock — JS-driven compare block input binding.
 *
 * Two bordered multi-selects (key columns, measurement columns) over the
 * columns common to both inputs, plus join-type and diff-metric dropdowns.
 * All four submit immediately.
 *
 * Pick order is preserved: key columns order the join and the leading output
 * columns, measurement columns order the diff columns after them. The chips
 * are drag-reorderable (the Blockr.Select.multi default).
 *
 * Depends on: blockr-core.js, blockr-select.js (both from blockr.dplyr)
 */
(() => {
  'use strict';

  // Plain strings: Blockr.Select shows the value and dims `label` beside it as
  // secondary text, so a label that merely re-cases the value reads as
  // "inner Inner". The metric labels below earn their place.
  const JOIN_TYPES = ['inner', 'full'];

  const METRICS = [
    { value: 'diff', label: 'Difference' },
    { value: 'abs_diff', label: 'Absolute difference' },
    { value: 'rel_diff', label: 'Relative difference (%)' },
    { value: 'ratio', label: 'Ratio' },
    { value: 'pct_change', label: 'Percent change' }
  ];

  /**
   * Block state as exchanged with R — mirrors the formals of
   * new_compare_block() / compare_frames().
   * @typedef {Object} CompareState
   * @property {string[]} key_cols Columns to join on, in join order
   * @property {string[]} measure_cols Numeric columns to diff, in output order
   * @property {string} join_type "inner" | "full"
   * @property {string} metric "diff" | "abs_diff" | "rel_diff" | "ratio" | "pct_change"
   */

  /** @param {unknown} v @returns {string[]} */
  const toArray = (v) => Array.isArray(v)
    ? /** @type {string[]} */ (v.slice())
    : (v == null || v === '' ? [] : [/** @type {string} */ (v)]);

  class CompareBlock {
    /** @param {HTMLElement} el */
    constructor(el) {
      this.el = el;
      /** @type {string[]} */
      this.key_cols = [];
      /** @type {string[]} */
      this.measure_cols = [];
      this.join_type = 'inner';
      this.metric = 'diff';
      /** @type {BlockrSelectOption[]} */
      this.columnOptions = [];
      /** @type {((value: boolean) => void) | null} */
      this._callback = null;
      this._submitted = false;
      /** @type {BlockrSelectMultiHandle | null} */
      this._keySelect = null;
      /** @type {BlockrSelectMultiHandle | null} */
      this._measureSelect = null;
      /** @type {BlockrSelectSingleHandle | null} */
      this._joinSelect = null;
      /** @type {BlockrSelectSingleHandle | null} */
      this._metricSelect = null;
      /** @type {HTMLElement} */
      this._grid = document.createElement('div');

      this._buildDOM();
    }

    /**
     * @param {string} label
     * @param {string} extraClass
     * @returns {HTMLElement} the div the select renders into
     */
    _field(label, extraClass) {
      const wrap = document.createElement('div');
      wrap.className = `compare-block-field ${extraClass}`;
      const lab = document.createElement('label');
      lab.className = 'blockr-label';
      lab.textContent = label;
      wrap.appendChild(lab);
      const host = document.createElement('div');
      host.className = 'blockr-select--bordered';
      wrap.appendChild(host);
      this._grid.appendChild(wrap);
      return host;
    }

    _buildDOM() {
      this._grid.className = 'compare-block-grid';

      const S = /** @type {BlockrSelectStatic} */ (Blockr.Select);

      this._keySelect = S.multi(this._field('Key columns', 'compare-block-keys'), {
        options: this.columnOptions,
        selected: this.key_cols,
        placeholder: 'Select columns…',
        onChange: (/** @type {string[]} */ vals) => {
          this.key_cols = vals;
          this._submit();
        }
      });

      this._measureSelect = S.multi(
        this._field('Measurement columns', 'compare-block-measures'), {
          options: this.columnOptions,
          selected: this.measure_cols,
          placeholder: 'Select columns…',
          onChange: (/** @type {string[]} */ vals) => {
            this.measure_cols = vals;
            this._submit();
          }
        }
      );

      this._joinSelect = S.single(this._field('Join type', 'compare-block-join'), {
        options: JOIN_TYPES,
        selected: this.join_type,
        onChange: (/** @type {string} */ val) => {
          this.join_type = val;
          this._submit();
        }
      });

      this._metricSelect = S.single(this._field('Diff metric', 'compare-block-metric'), {
        options: METRICS,
        selected: this.metric,
        onChange: (/** @type {string} */ val) => {
          this.metric = val;
          this._submit();
        }
      });

      this.el.appendChild(this._grid);
    }

    /** @returns {CompareState} */
    _compose() {
      return {
        key_cols: this.key_cols.slice(),
        measure_cols: this.measure_cols.slice(),
        join_type: this.join_type,
        metric: this.metric
      };
    }

    _submit() {
      this._submitted = true;
      this._callback?.(true);
    }

    /**
     * Null before the first user edit: R owns the constructor/restore state
     * and must not be clobbered by the binding's initial read.
     */
    getValue() {
      if (!this._submitted) return null;
      return this._compose();
    }

    /**
     * Columns common to both inputs, as `{name, label}` records
     * (blockr.dplyr's column-metadata protocol).
     * @param {BlockrPickerColumn[]} meta
     */
    updateColumns(meta) {
      this.columnOptions = (meta || []).map(
        (col) => ({ value: col.name, label: col.label || '' })
      );
      // setOptions() drops picks that no longer exist; mirror the surviving
      // selection back so _compose() never ships a stale column name.
      this._keySelect?.setOptions(this.columnOptions, this.key_cols);
      this.key_cols = this._keySelect ? this._keySelect.getValue() : this.key_cols;
      this._measureSelect?.setOptions(this.columnOptions, this.measure_cols);
      this.measure_cols = this._measureSelect
        ? this._measureSelect.getValue()
        : this.measure_cols;
    }

    /**
     * Rebuild from R's state. Never fires the callback.
     * @param {Partial<CompareState> | null | undefined} state
     */
    setState(state) {
      this.key_cols = toArray(state?.key_cols);
      this.measure_cols = toArray(state?.measure_cols);
      this.join_type = state?.join_type || 'inner';
      this.metric = state?.metric || 'diff';

      this._keySelect?.setOptions(this.columnOptions, this.key_cols);
      this._measureSelect?.setOptions(this.columnOptions, this.measure_cols);
      this._joinSelect?.setOptions(JOIN_TYPES, this.join_type);
      this._metricSelect?.setOptions(METRICS, this.metric);
    }
  }

  Blockr.registerBlock({
    name: 'compare',
    Block: CompareBlock,
    messages: {
      'compare-columns': (block, msg) => block.updateColumns(msg.columns),
      'compare-block-update': (block, msg) => block.setState(msg.state)
    }
  });
})();
