function vit(message) {
  var path = message.test;
  var data = JSON.stringify(message.data);

  var url = `https://vit.inzight.nz/${path}.php?file=JSON`;

  var form = document.createElement("form");
  form.action = url;
  form.method = "post";
  form.target = "vit-frame";

  var input = document.createElement("input");
  input.type = "hidden";
  input.name = "p_data";
  input.value = data;
  form.appendChild(input);

  console.log(message.vars);
  for (const i in message.vars) {
    var var_param = document.createElement("input");
    var_param.type = "hidden";
    var_param.name = "var";
    var_param.value = message.vars[i];
    console.log(message.vars[i]);

    form.appendChild(var_param);
  }

  document.body.appendChild(form);
  form.submit();
}

Shiny.addCustomMessageHandler("send_to_vit_frame", vit);
