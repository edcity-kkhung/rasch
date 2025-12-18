
import React, { useState } from 'react';

const backendHostPort = import.meta.env.VITE_BACKEND_SERVER_HOST_PORT;
const API_URL_PREFIX = `http://${backendHostPort}/api`;
const API_MODEL_FIT = `${API_URL_PREFIX}/rasch_model_fit`;
const API_ESTIMATE_ABILITY = `${API_URL_PREFIX}/rasch_model_estimate_ability`;

function parseIntegers(input) {
  // Accept commas, spaces, newlines; ignore extra separators
  if (typeof input !== 'string') return [];
  const tokens = input
    .split(/[\s,]+/)
    .map(t => t.trim())
    .filter(t => t.length > 0);

  const nums = [];
  for (const t of tokens) {
    const v = Number(t);
    if (!Number.isFinite(v) || !Number.isInteger(v)) {
      throw new Error(`Invalid integer in data_vector: "${t}"`);
    }
    nums.push(v);
  }
  return nums;
}

export default function App() {
  const [dataText, setDataText] = useState('1,0,0,0,0,0,1,0,0,0,1,1,1,0,0,1,1,1,1,0,0,1,1,1,1'); // demo data
  const [nrow, setNrow] = useState(5);
  const [ncol, setNcol] = useState(5);
  const [responseText, setResponseText] = useState('1,0,0,0,1');

  const [loading, setLoading] = useState(false);
  const [error, setError] = useState('');
  const [result, setResult] = useState(null);
  const [estimateResult, setEstimateResult] = useState(null);

  const handleSubmit = async (e) => {
    e.preventDefault();
    setError('');
    setResult(null);

    let data_vector = [];
    try {
      data_vector = parseIntegers(dataText);
    } catch (err) {
      setError(err.message);
      return;
    }

    if (!Number.isInteger(Number(nrow)) || Number(nrow) <= 0) {
      setError('nrow must be a positive integer');
      return;
    }
    if (!Number.isInteger(Number(ncol)) || Number(ncol) <= 0) {
      setError('ncol must be a positive integer');
      return;
    }

    const rn = Number(nrow);
    const cn = Number(ncol);

    if (data_vector.length !== rn * cn) {
      setError(
        `data_vector length (${data_vector.length}) must equal nrow*ncol (${rn * cn}).`
      );
      return;
    }

    setLoading(true);
    try {
      const resp = await fetch(API_MODEL_FIT, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({
          data_vector,
          nrow: rn,
          ncol: cn,
        }),
      });

      if (!resp.ok) {
        const msg = await resp.text();
        throw new Error(
          `HTTP ${resp.status}: ${msg || 'Request failed'}`
        );
      }

      const json = await resp.json();
      // Expecting: { difficulties: number[], abilities: number[] }
      if (
        !json ||
        !Array.isArray(json.difficulties) ||
        !Array.isArray(json.abilities)
      ) {
        throw new Error('Unexpected API response shape.');
      }
      setResult(json);
    } catch (err) {
      setError(err.message || String(err));
    } finally {
      setLoading(false);
    }
  };

  const handleEstimateAbility = async (e) => {
    e.preventDefault();
    setError('');
    setEstimateResult(null);
    
    let responses_vector = [];
    let item_difficulties = result ? result.difficulties : null;
    if (!item_difficulties) {
      setError('Item difficulties not available. Please fit the model first.');
      return;
    }
    try {
      responses_vector = parseIntegers(responseText);
    } catch (err) {
      setError(err.message);
      return;
    }

    setLoading(true);
    try {
      const resp = await fetch(API_ESTIMATE_ABILITY, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({
          responses_vector,
          item_difficulties,
        }),
      });
      if (!resp.ok) {
        const msg = await resp.text();
        throw new Error(
          `HTTP ${resp.status}: ${msg || 'Request failed'}`
        );
      }

      const json = await resp.json();
      // Expecting: { ability: number[] }
      if (!json || !Array.isArray(json.ability)) {
        throw new Error('Unexpected API response shape.');
      }
      setEstimateResult(json);
    } catch (err) {
      setError(err.message || String(err));
    } finally {
      setLoading(false);
    }
  };

  const exampleMatrixInfo = () => {
    const rn = Number(nrow);
    const cn = Number(ncol);
    if (!rn || !cn) return '—';
    return `${rn} rows × ${cn} cols = ${rn * cn} integers`;
  };

  return (
    <div style={styles.container}>
      <h1 style={styles.title}>Rasch Model Fit Client</h1>

      <form onSubmit={handleSubmit} style={styles.form}>
        <div style={styles.field}>
          <label htmlFor="nrow" style={styles.label}>Number of Students (rows)</label>
          <input
            id="nrow"
            type="number"
            min={1}
            value={nrow}
            onChange={e => setNrow(e.target.value)}
            style={styles.input}
          />
        </div>

        <div style={styles.field}>
          <label htmlFor="ncol" style={styles.label}>Number of Items (columns)</label>
          <input
            id="ncol"
            type="number"
            min={1}
            value={ncol}
            onChange={e => setNcol(e.target.value)}
            style={styles.input}
          />
        </div>

        <div style={styles.field}>
          <label htmlFor="data_vector" style={styles.label}>
            data_vector (Serialize Matrix in rows) ({exampleMatrixInfo()})
          </label>
          <textarea
            id="data_vector"
            rows={6}
            placeholder="Enter integers separated by spaces/commas/newlines"
            value={dataText}
            onChange={e => setDataText(e.target.value)}
            style={styles.textarea}
          />
          <small style={styles.hint}>
            Example (2×3): <code>1 0 1  1 1 0</code> or <code>1,0,1,1,1,0</code>
          </small>
        </div>

        <button type="submit" style={styles.button} disabled={loading}>
          {loading ? 'Submitting…' : 'Submit'}
        </button>
      </form>

      {error && (
        <div style={styles.error}>
          <strong>Error:</strong> {error}
        </div>
      )}

      {result && (
        <div style={styles.card}>
          <h2>API Result</h2>
          <div style={styles.arrayBlock}>
            <h3>Item Difficulties</h3>
            <pre style={styles.pre}>
{JSON.stringify(result.difficulties, null, 2)}
            </pre>
          </div>
          <div style={styles.arrayBlock}>
            <h3>Student Abilities</h3>
            <pre style={styles.pre}>
{JSON.stringify(result.abilities, null, 2)}
            </pre>
          </div>

          <form onSubmit={handleEstimateAbility} style={styles.form}>
            <div style={styles.field}>
              <label htmlFor="data_response" style={styles.label}>
                data_response (Responses for a single student)
              </label>
              <textarea
                id="data_response"
                rows={6}
                placeholder="Enter integers separated by spaces/commas/newlines"
                value={responseText}
                onChange={e => setResponseText(e.target.value)}
                style={styles.textarea}
              />
              <small style={styles.hint}>
                Example : <code>1 0 1 1 0</code> or <code>1,0,1,1,0</code>
              </small>
            </div>

            <button type="submit" style={styles.button} disabled={loading}>
              {loading ? 'Estimating…' : 'Estimate'}
            </button>
          </form>
          {estimateResult && (
            <div style={styles.arrayBlock}>
              <h3>Estimated Ability</h3>
              <pre style={styles.pre}>
{JSON.stringify(estimateResult.ability, null, 2)}
              </pre>
            </div>
          )}
        </div>
      )}
    </div>
  );
}

const styles = {
  container: {
    maxWidth: 900,
    margin: '0 auto',
    padding: '2rem',
    fontFamily: 'system-ui, -apple-system, Segoe UI, Roboto, Helvetica, Arial, sans-serif',
  },
  title: {
    marginTop: 0,
    marginBottom: '1rem',
  },
  form: {
    display: 'grid',
    gridTemplateColumns: '1fr',
    gap: '1rem',
    marginBottom: '1rem',
  },
  field: { display: 'flex', flexDirection: 'column' },
  label: { fontWeight: 600, marginBottom: '0.25rem' },
  input: {
    padding: '0.5rem',
    fontSize: '1rem',
    border: '1px solid #ccc',
    borderRadius: 6,
  },
  textarea: {
    padding: '0.5rem',
    fontSize: '1rem',
    border: '1px solid #ccc',
    borderRadius: 6,
    resize: 'vertical',
  },
  hint: { color: '#666' },
  button: {
    padding: '0.6rem 1rem',
    fontSize: '1rem',
    borderRadius: 6,
    border: '1px solid #0078d4',
    color: 'white',
    backgroundColor: '#0078d4',
    cursor: 'pointer',
  },
  error: {
    backgroundColor: '#ffecec',
    color: '#b00020',
    padding: '0.75rem',
    borderRadius: 6,
    marginTop: '0.5rem',
  },
  card: {
    border: '1px solid #e5e5e5',
    borderRadius: 8,
    padding: '1rem',
    marginTop: '1rem',
    backgroundColor: '#fafafa',
  },
  arrayBlock: { marginTop: '0.5rem' },
  pre: {
    background: '#111',
    color: '#0f0',
    padding: '0.75rem',
    borderRadius: 6,
    overflowX: 'auto',
  },
};
