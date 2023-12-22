function isNumber(n) {
  return !isNaN(parseFloat(n)) && isFinite(n);
}

function testNumeric(column) {
  for (i = 0; i < column.length; i++) {
    if (!isNumber(column[i])) {
      return false;
    }
  }
  return true;
}
//var cars = ["1", "2.2", "3.2"];
//alert(String(testNumeric(cars)))
