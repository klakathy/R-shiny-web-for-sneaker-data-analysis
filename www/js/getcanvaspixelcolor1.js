﻿if (typeof HTMLElement == 'undefined'){
	var HTMLElement = function(){};
	if (window.webkit) document.createElement("iframe"); //fixes safari
	HTMLElement.prototype = (window.webkit) ? window["[[DOMElement.prototype]]"] : {};
}
HTMLElement.prototype.getPixelColor = function(x, y){
	var thisContext = this.getContext("2d");
	var imageData = thisContext.getImageData(x, y, 1, 1);

	var pixel = imageData.data;
	var r = pixel[0];
	var g = pixel[1];
	var b = pixel[2];
	var a = pixel[3] / 255
	a = Math.round(a * 100) / 100;
	var rHex = r.toString(16);
	r < 16 && (rHex = "0" + rHex);
	var gHex = g.toString(16);
	g < 16 && (gHex = "0" + gHex);
	var bHex = b.toString(16);
	b < 16 && (bHex = "0" + bHex);
	var rgbaColor = [r,g,b,a];
	var rgbColor = [r,g,b];
	var hexColor = "#" + rHex + gHex + bHex;
	return {
		rgba : rgbaColor,
		rgb : rgbColor,
		hex : hexColor,
		r : r,
		g : g,
		b : b,
		a : a
	};
}
function show7(){
	console.log("pix OKOKOKOKOKO");
}
