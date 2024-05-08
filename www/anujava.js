shinyjs.init = function(){
  $('#tbs li a[data-value="tab_4"]').hide();
  $('#tbs li a[data-value="tab_5"]').hide();
}

shinyjs.disableTab = function(name) {
    var tab = $('#tbs li a[data-value=' + name + ']');
    tab.hide();
}

shinyjs.enableTab = function(name) {
    var tab = $('#tbs li a[data-value=' + name + ']');
    tab.show();
}

shinyjs.updateVis = function(val){
	
	html("viscondition", val)
	
}