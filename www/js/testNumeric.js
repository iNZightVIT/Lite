function testNumeric(column) {
	alert(String(!isNaN(parseFloat(column)) && isFinite(column)));
}
var cars = [1.2, 2.2, 3.2];
testNumeric(cars);
