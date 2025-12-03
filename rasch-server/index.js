
const express = require('express');
const cors = require('cors');
const helmet = require('helmet');
const morgan = require('morgan');
const R = require('r-integration');

const app = express();
const PORT = process.env.PORT || 3000;

app.use(cors());
app.use(express.json({ limit: '1mb' }));
app.use(helmet());
app.use(morgan('tiny'));

function validateBody(body) {
    const { data_vector, nrow, ncol } = body || {};

    if (!Array.isArray(data_vector)) {
        return 'data_vector must be an array of integers';
    }
    if (!Number.isInteger(nrow) || nrow <= 0) {
        return 'nrow must be a positive integer';
    }
    if (!Number.isInteger(ncol) || ncol <= 0) {
        return 'ncol must be a positive integer';
    }
    if (data_vector.length !== nrow * ncol) {
        return `data_vector length (${data_vector.length}) must equal nrow*ncol (${nrow * ncol})`;
    }
    // Ensure elements are integers
    for (const v of data_vector) {
        if (!Number.isFinite(v) || !Number.isInteger(v)) {
            return 'data_vector must contain only finite integers';
        }
    }
    return null;
}

/**
 * POST /api/rasch_model_fit
 * Body: { data_vector: number[], nrow: number, ncol: number }
 * Response: { abilities: number[], difficulties: number[] }
 */
app.post('/api/rasch_model_fit', (req, res) => {
    const err = validateBody(req.body);
    if (err) {
        return res.status(400).json({ error: err });
    }

    const { data_vector, nrow, ncol } = req.body;

    R.callMethodAsync(
        "./rasch_tools.R",
        "rasch_model_fit",
        {
            data_vector: data_vector,
            nrow: nrow,
            ncol: ncol,
        },
        // "C:\\Users\\kk.hung\\AppData\\Local\\Programs\\R"
    )
        .then((result) => {
            res.json({
                abilities: result[0].abilities,
                difficulties: result[0].difficulties,
            });
        })
        .catch((e) => {
            console.error(e);
            res.status(500).json({ error: 'Internal Server Error' });
        });
});

app.get('/health', (_req, res) => {
    res.json({ status: 'ok', time: new Date().toISOString() });
});

app.listen(PORT, () => {
    console.log(`API listening on port ${PORT}`);
});

