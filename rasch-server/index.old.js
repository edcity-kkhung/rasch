const express = require('express');
const app = express();
const port = process.env.PORT || 3000;

// In NodeJS
const R = require('r-integration');

// let result = R.executeRCommand("max(1,2,3)", "C:\\Users\\kk.hung\\AppData\\Local\\Programs\\R");
// let result = R.executeRScript("./rasch.R", "C:\\Users\\kk.hung\\AppData\\Local\\Programs\\R");
// let result = R.callMethod("./rasch_tools.R", "score_person", {response_vector: [1,0,1,1,0,1,1,1]}, "C:\\Users\\kk.hung\\AppData\\Local\\Programs\\R");
// let response_vector = [0, 0, 0, 0, 0, 0, 0, 1];
// let response_vector = [1,0,0,0,0,0,0,0];
// let response_vector = [1,0,1,1,0,1,1,1];
// let response_vector = [1,0,1,1,1,1,1,1];
// let response_vector = [1,1,1,1,1,1,1,1];

// let result = R.callMethod(
//     "./rasch_tools.R",
//     "score_rank_person",
//     {
//         response_vector: response_vector
//     },
//     // "C:\\Users\\kk.hung\\AppData\\Local\\Programs\\R"
// );
// console.log(result);

// let result = R.executeRScript("./rasch_tools.R", "C:\\Users\\kk.hung\\AppData\\Local\\Programs\\R");
// let result = R.callMethod(
//     "./rasch_tools.R",
//     "rasch_model_fit_withRandData",
//     {
//       numItem: 5,
//       numPerson: 50,
//     },
//     "C:\\Users\\kk.hung\\AppData\\Local\\Programs\\R"
// );
let result = R.callMethod(
    "./rasch_tools.R",
    "rasch_model_fit",
    {
      // data_vector: [1,0,1,1,0,1,1,1,1,0,1,0,1,1,0,1,1,1,0,1,1,1,0,1,1],
      data_vector: [1,0,0,0,0,
                    0,1,0,0,0,
                    1,1,1,0,0,
                    1,1,1,1,0,
                    0,1,1,1,1],
      nrow: 5,
      ncol: 5,
    },
    "C:\\Users\\kk.hung\\AppData\\Local\\Programs\\R"
);
console.log(result[0].difficulties);
console.log(result[0].abilities);

app.get('/', (req, res) => {
  res.send('Hello from Docker!');
});

app.listen(port, () => {
  console.log(`App listening at http://localhost:${port}`);
});
