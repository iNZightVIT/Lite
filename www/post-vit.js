function to_vit(path) {
	return function(message) {
	    var url = `https://vit-test123.herokuapp.com/${path}.php?file=JSON`;
	    var to_send = JSON.stringify(message);
	    // console.log(to_send);
	    var form = $(`<form action="${url}" method="post" target="_blank"><input type="hidden" name="p_data" value='${to_send}' /></form>`);
	    $('body').append(form);
	    form.submit(); 
	}
}

var to_vit_randomisation_test = to_vit("randomisationTest/RVar");
var to_vit_sampling_variation = to_vit("samplingVariation/SamplingVariation");
var to_vit_bootstrap = to_vit("bootstrap/bootstrap");
var to_vit_randomisation_variation = to_vit("RandomisationVar/RVar");

Shiny.addCustomMessageHandler('to_vit_randomisation_test', to_vit_randomisation_test);
Shiny.addCustomMessageHandler('to_vit_sampling_variation', to_vit_sampling_variation);
Shiny.addCustomMessageHandler('to_vit_bootstrap', to_vit_bootstrap);
Shiny.addCustomMessageHandler('to_vit_randomisation_variation', to_vit_randomisation_variation);
