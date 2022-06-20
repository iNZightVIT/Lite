function to_vit(path) {
	return function(message) {
	    var url = `https://vit-test123.herokuapp.com/${path}.php?file=JSON`;
	    var to_send = JSON.stringify(message);
	    var form = $(`<form action="${url}" method="post" target="vit-frame"><input type="hidden" name="p_data" value='${to_send}' /></form>`);
	    $('body').append(form);
	    form.submit(); 
	}
}

var vit_randomisation_test = to_vit("randomisationTest/RVar");
var vit_bootstrap = to_vit("bootstrap/bootstrap");

Shiny.addCustomMessageHandler('data_for_randomisation', vit_randomisation_test);
Shiny.addCustomMessageHandler('vit_bootstrap', vit_bootstrap);
