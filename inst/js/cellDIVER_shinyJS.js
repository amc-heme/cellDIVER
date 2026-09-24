shinyjs.getTopScroll = function(params){
    // Define default params, set defaults if undefined
    var defaultParams = {
        // ID of element to get scroll position
        targetId : null,
        // Input ID for storing recorded scroll position 
        // Use namespacing if the function is called within a module
        inputId : null
    };

    params = shinyjs.getParams(params, defaultParams);
    
    // select element via jQuery
    var elem = $("#" + params.targetId);
    
    // Get scroll value of element (px) and send to shiny
    var scrollPos = elem.scrollTop();

    Shiny.onInputChange(params.inputId, scrollPos);
    };

shinyjs.setTopScroll = function(params){
    // Define default params, set defaults if undefined
    var defaultParams = {
        // Element to set scroll value
        targetId : null,
        // Input Id used to restore scroll value
        restoreId : null,
    };

    params = shinyjs.getParams(params, defaultParams);

    var restoreVal = Shiny.shinyapp.$inputValues[params.restoreId]

    // select element via jQuery
    var elem = $("#" + params.targetId);

    // Set scroll position by calling the scrollTop
    // method with the position value
    elem.scrollTop(restoreVal);
}
