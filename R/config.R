#### defaults ####
DEFAULTS <- list(
  period_start = 2000,
  period_end = 2025,

  age_start = 0,
  age_end = 105,
  intervals = 5
)

#### CSS ####
APP_CSS <- "
/* card */
.card-style {
  background-color: #ffffff;
  border: 2px solid #333;
  border-radius: 5px;
  box-shadow: 5px 5px 0px rgba(0,0,0,0.2);
  padding: 20px;
  margin-bottom: 20px;
  height: 100%;
}

/* run button */
#run_btn {
  width: 100%;
  font-size: 18px;
  font-weight: bold;
  margin-top: 20px;
  height: 50px;
}

h3 {
  margin-top: 0px;
  border-bottom: 1px solid #eee;
  padding-bottom: 10px;
  margin-bottom: 20px;
}

/* pop-up window */
#confirm_age { background-color: #d9534f; color: white; }
#cancel_age { background-color: #5bc0de; color: white; }

/* formula tags */
.formula-tags-container {
  border: 1px solid #ccc;
  border-radius: 4px;
  padding: 6px 8px;
  min-height: 38px;
  background-color: #fff;
  display: flex;
  flex-wrap: wrap;
  gap: 5px;
  align-items: center;
}
.formula-tag {
  display: inline-flex;
  align-items: center;
  background-color: #e8f0fe;
  color: #333;
  border: 1px solid #b0c4de;
  border-radius: 3px;
  padding: 2px 6px;
  font-size: 13px;
  white-space: nowrap;
}
.formula-tag .tag-remove {
  margin-left: 5px;
  cursor: pointer;
  color: #999;
  font-weight: bold;
  font-size: 14px;
  line-height: 1;
}
.formula-tag .tag-remove:hover {
  color: #d9534f;
}
.formula-tag-plus {
  color: #999;
  font-size: 13px;
  margin: 0 1px;
}
"
