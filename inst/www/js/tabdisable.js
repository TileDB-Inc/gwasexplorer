// https://stackoverflow.com/questions/31703241
shinyjs.disableTab = function(name) {
  var tab = $('.nav li a[data-value=' + name + ']');
  tab.bind('click.tab', function(e) {
    e.preventDefault();
    return false;
  });
  tab.addClass('disabled');
}

shinyjs.enableTab = function(name) {
  var tab = $('.nav li a[data-value=' + name + ']');
  tab.unbind('click.tab');
  tab.removeClass('disabled');
}

shinyjs.init = function() {
  console.log("Disabling query tab");
  var tabs = ["Query", "Results", "Plot", "Stats", "Snippets"];
  for (var i = 0; i < tabs.length; i++) {
    shinyjs.disableTab(tabs[i]);
  }
}
