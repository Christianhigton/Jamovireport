'use client';

import { useEffect, useMemo, useRef, useState } from 'react';

const standalone = [
  ['eduTTest', 'T-Tests', 'General guided t-test'],
  ['eduTTestIndependent', 'T-Tests', 'Independent-samples t-test'],
  ['eduTTestPaired', 'T-Tests', 'Paired-samples t-test'],
  ['eduAnova', 'ANOVA', 'One-way ANOVA'],
  ['eduBetweenAnova', 'ANOVA', 'Between-subjects ANOVA'],
  ['eduAncova', 'ANOVA', 'ANCOVA'],
  ['eduRMAnova', 'ANOVA', 'Repeated-measures ANOVA'],
  ['eduMixedAnova', 'ANOVA', 'Mixed ANOVA'],
  ['eduMancova', 'ANOVA', 'MANOVA / MANCOVA'],
  ['eduCorrelation', 'Association', 'Correlation'],
  ['eduRegression', 'Regression', 'Linear regression'],
  ['eduLogistic', 'Regression', 'Binary logistic regression'],
  ['eduMultinomialLogistic', 'Regression', 'Multinomial logistic regression'],
  ['eduChiSquareIndependence', 'Frequencies', 'Chi-square test of independence'],
  ['eduChiSquareGoodness', 'Frequencies', 'Chi-square goodness-of-fit'],
  ['eduReliabilityOmega', 'Reliability', 'Omega and alpha reliability'],
  ['eduDemographics', 'Descriptives', 'Demographic table and paragraph'],
].map(([id, family, title]) => ({ id, family, title, mode: 'standalone' }));

const addons = [
  ['jrReportTTestIS', 'T-Tests', 'Independent-samples t-test add-on'],
  ['jrReportTTestPS', 'T-Tests', 'Paired-samples t-test add-on'],
  ['jrReportAnovaOneW', 'ANOVA', 'One-way ANOVA add-on'],
  ['jrReportAnova', 'ANOVA', 'Factorial ANOVA add-on'],
  ['jrReportAncova', 'ANOVA', 'ANCOVA add-on'],
  ['jrReportAnovaRM', 'ANOVA', 'Repeated-measures ANOVA add-on'],
  ['jrReportMancova', 'ANOVA', 'MANCOVA add-on'],
  ['jrReportCorrMatrix', 'Association', 'Correlation matrix add-on'],
  ['jrReportLinReg', 'Regression', 'Linear regression add-on'],
  ['jrReportLogRegBin', 'Regression', 'Binary logistic regression add-on'],
  ['jrReportLogRegMulti', 'Regression', 'Multinomial logistic regression add-on'],
  ['jrReportContTables', 'Frequencies', 'Contingency tables add-on'],
  ['jrReportPropTestN', 'Frequencies', 'Proportion test add-on'],
  ['jrReportReliability', 'Reliability', 'Reliability add-on'],
].map(([id, family, title]) => ({ id, family, title, mode: 'addon' }));

const modules = [...standalone, ...addons];

const checks = [
  ['opens', 'Opens and runs', 'The analysis opens, accepts valid variables and completes without an error.'],
  ['normal', 'Normal data', 'Produces complete results with the supplied QA data.'],
  ['problem', 'Problem data', 'Handles missing, skewed, constant or collinear data safely.'],
  ['statistics', 'Statistics match', 'Statistics, df, p, confidence intervals and effect sizes match core jamovi.'],
  ['direction', 'Labels and direction', 'Variables, groups, reference levels, signs and directions are correct.'],
  ['assumptions', 'Assumptions attributed', 'Every assumption clearly identifies the analysis or outcome it applies to.'],
  ['reporting', 'Reporting is clean', 'No repetition, contradiction, placeholder, template fragment or misleading claim.'],
  ['layout', 'Tables and layout fit', 'Tables fit widthways and all explanatory content remains accessible.'],
  ['copy', 'Copy buttons', 'Each button copies only its own complete text section.'],
  ['updates', 'Updates cleanly', 'Changing variables or options replaces output without stale or duplicated content.'],
  ['reopen', 'Save and reopen', 'The saved .omv reopens with the same settings, statistics and visible output.'],
];

const statusOptions = [
  ['not-tested', 'Not tested'],
  ['pass', 'Pass'],
  ['fail', 'Fail'],
  ['blocked', 'Blocked'],
];

const storageKey = 'jreport-qa-checklist-v1';

function freshState() {
  return {
    meta: { tester: '', jamovi: '2.7.24', build: 'jReport 1.0.0', date: new Date().toISOString().slice(0, 10) },
    modules: Object.fromEntries(modules.map((m) => [
      m.id,
      { status: 'not-tested', checks: {}, notes: '', bugId: '', updatedAt: '' },
    ])),
  };
}

function mergeState(saved) {
  const base = freshState();
  if (!saved || typeof saved !== 'object') return base;
  return {
    meta: { ...base.meta, ...(saved.meta || {}) },
    modules: Object.fromEntries(modules.map((m) => [
      m.id,
      { ...base.modules[m.id], ...(saved.modules?.[m.id] || {}) },
    ])),
  };
}

function ModuleCard({ module, value, onChange }) {
  const complete = checks.filter(([key]) => value.checks[key]).length;
  const percent = Math.round((complete / checks.length) * 100);
  return (
    <article className={`module-card status-${value.status}`}>
      <div className="card-head">
        <div>
          <div className="eyebrow">{module.family} · {module.mode === 'addon' ? 'Automatic add-on' : 'Standalone'}</div>
          <h3>{module.title}</h3>
          <code>{module.id}</code>
        </div>
        <div className="mini-progress" aria-label={`${complete} of ${checks.length} checks complete`}>
          <strong>{percent}%</strong>
          <span>{complete}/{checks.length}</span>
        </div>
      </div>

      <div className="status-picker" role="group" aria-label={`Status for ${module.title}`}>
        {statusOptions.map(([key, label]) => (
          <button
            key={key}
            className={value.status === key ? `selected ${key}` : ''}
            onClick={() => onChange({ ...value, status: key, updatedAt: new Date().toISOString() })}
            type="button"
          >
            {label}
          </button>
        ))}
      </div>

      <div className="check-grid">
        {checks.map(([key, label, help]) => (
          <label className="check-row" key={key} title={help}>
            <input
              type="checkbox"
              checked={Boolean(value.checks[key])}
              onChange={(event) => onChange({
                ...value,
                checks: { ...value.checks, [key]: event.target.checked },
                updatedAt: new Date().toISOString(),
              })}
            />
            <span className="custom-check" aria-hidden="true">✓</span>
            <span><strong>{label}</strong><small>{help}</small></span>
          </label>
        ))}
      </div>

      <div className="card-fields">
        <label>
          <span>Bug ID</span>
          <input
            value={value.bugId}
            placeholder="e.g. JR-014"
            onChange={(e) => onChange({ ...value, bugId: e.target.value })}
          />
        </label>
        <label className="notes-field">
          <span>Notes and evidence</span>
          <textarea
            value={value.notes}
            placeholder="Record unexpected behaviour, exact values, screenshot name, or .omv filename…"
            onChange={(e) => onChange({ ...value, notes: e.target.value })}
          />
        </label>
      </div>
    </article>
  );
}

export default function Home() {
  const [state, setState] = useState(freshState);
  const [ready, setReady] = useState(false);
  const [mode, setMode] = useState('all');
  const [status, setStatus] = useState('all');
  const [query, setQuery] = useState('');
  const [onlyIncomplete, setOnlyIncomplete] = useState(false);
  const fileRef = useRef(null);

  useEffect(() => {
    try {
      const saved = JSON.parse(localStorage.getItem(storageKey));
      setState(mergeState(saved));
    } catch {
      setState(freshState());
    }
    setReady(true);
  }, []);

  useEffect(() => {
    if (ready) localStorage.setItem(storageKey, JSON.stringify(state));
  }, [state, ready]);

  const stats = useMemo(() => {
    const values = Object.values(state.modules);
    const totalChecks = modules.length * checks.length;
    const completeChecks = values.reduce(
      (sum, item) => sum + checks.filter(([key]) => item.checks[key]).length, 0
    );
    return {
      pass: values.filter((x) => x.status === 'pass').length,
      fail: values.filter((x) => x.status === 'fail').length,
      blocked: values.filter((x) => x.status === 'blocked').length,
      tested: values.filter((x) => x.status !== 'not-tested').length,
      totalChecks,
      completeChecks,
      percent: Math.round((completeChecks / totalChecks) * 100),
    };
  }, [state]);

  const shown = useMemo(() => modules.filter((module) => {
    const item = state.modules[module.id];
    const searchable = `${module.id} ${module.title} ${module.family}`.toLowerCase();
    const incomplete = checks.some(([key]) => !item.checks[key]);
    return (mode === 'all' || module.mode === mode)
      && (status === 'all' || item.status === status)
      && (!onlyIncomplete || incomplete)
      && searchable.includes(query.toLowerCase());
  }), [mode, status, onlyIncomplete, query, state]);

  function updateModule(id, value) {
    setState((current) => ({
      ...current,
      modules: { ...current.modules, [id]: value },
    }));
  }

  function exportData() {
    const blob = new Blob([JSON.stringify(state, null, 2)], { type: 'application/json' });
    const url = URL.createObjectURL(blob);
    const anchor = document.createElement('a');
    anchor.href = url;
    anchor.download = `jreport-qa-${state.meta.date || 'checklist'}.json`;
    anchor.click();
    URL.revokeObjectURL(url);
  }

  function importData(event) {
    const file = event.target.files?.[0];
    if (!file) return;
    const reader = new FileReader();
    reader.onload = () => {
      try {
        setState(mergeState(JSON.parse(reader.result)));
      } catch {
        window.alert('That file is not a valid jReport QA export.');
      }
    };
    reader.readAsText(file);
    event.target.value = '';
  }

  function resetData() {
    if (window.confirm('Reset every status, tick, note and bug ID? This cannot be undone unless you exported a backup.')) {
      setState(freshState());
    }
  }

  if (!ready) return null;

  return (
    <main>
      <header className="hero">
        <div className="hero-copy">
          <div className="brand-mark">jR</div>
          <div>
            <p className="kicker">Release quality workspace</p>
            <h1>jReport QA Checklist</h1>
            <p className="lede">
              Work through every standalone analysis and automatic jamovi add-on.
              Your progress saves automatically in this browser.
            </p>
          </div>
        </div>
        <div className="hero-actions">
          <button className="secondary" onClick={exportData}>Export backup</button>
          <button className="secondary" onClick={() => fileRef.current?.click()}>Import</button>
          <input ref={fileRef} type="file" accept="application/json" hidden onChange={importData} />
        </div>
      </header>

      <section className="meta-panel">
        <label><span>Tester</span><input value={state.meta.tester} placeholder="Your name" onChange={(e) => setState({ ...state, meta: { ...state.meta, tester: e.target.value } })} /></label>
        <label><span>jamovi version</span><input value={state.meta.jamovi} onChange={(e) => setState({ ...state, meta: { ...state.meta, jamovi: e.target.value } })} /></label>
        <label><span>jReport build</span><input value={state.meta.build} onChange={(e) => setState({ ...state, meta: { ...state.meta, build: e.target.value } })} /></label>
        <label><span>Test date</span><input type="date" value={state.meta.date} onChange={(e) => setState({ ...state, meta: { ...state.meta, date: e.target.value } })} /></label>
      </section>

      <section className="dashboard">
        <div className="progress-card">
          <div className="progress-ring" style={{ '--progress': `${stats.percent * 3.6}deg` }}>
            <div><strong>{stats.percent}%</strong><span>complete</span></div>
          </div>
          <div>
            <p className="kicker">Overall progress</p>
            <h2>{stats.completeChecks} of {stats.totalChecks} checks</h2>
            <p>{stats.tested} of {modules.length} analyses have a status.</p>
          </div>
        </div>
        <div className="metric pass"><span>Passed</span><strong>{stats.pass}</strong></div>
        <div className="metric fail"><span>Failed</span><strong>{stats.fail}</strong></div>
        <div className="metric blocked"><span>Blocked</span><strong>{stats.blocked}</strong></div>
      </section>

      <section className="release-rule">
        <strong>Release rule</strong>
        <span>No Critical or High bugs, no failed reporting checks, and every analysis tested with normal and problematic data.</span>
      </section>

      <section className="toolbar">
        <div className="tabs">
          {[
            ['all', `All (${modules.length})`],
            ['standalone', `Standalone (${standalone.length})`],
            ['addon', `Add-ons (${addons.length})`],
          ].map(([key, label]) => <button key={key} className={mode === key ? 'active' : ''} onClick={() => setMode(key)}>{label}</button>)}
        </div>
        <div className="filters">
          <input className="search" value={query} onChange={(e) => setQuery(e.target.value)} placeholder="Search analyses…" />
          <select value={status} onChange={(e) => setStatus(e.target.value)}>
            <option value="all">All statuses</option>
            {statusOptions.map(([key, label]) => <option value={key} key={key}>{label}</option>)}
          </select>
          <label className="incomplete-toggle"><input type="checkbox" checked={onlyIncomplete} onChange={(e) => setOnlyIncomplete(e.target.checked)} /> Incomplete only</label>
        </div>
      </section>

      <div className="results-count">Showing {shown.length} analyses</div>
      <section className="module-list">
        {shown.map((module) => (
          <ModuleCard
            key={module.id}
            module={module}
            value={state.modules[module.id]}
            onChange={(value) => updateModule(module.id, value)}
          />
        ))}
      </section>

      <footer>
        <div>
          <strong>jReport QA</strong>
          <p>Autosaved locally. Export a JSON backup before resetting or changing browsers.</p>
        </div>
        <button className="danger-link" onClick={resetData}>Reset checklist</button>
      </footer>
    </main>
  );
}
