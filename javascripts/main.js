/* Set up map and display geojson */

var map = L.map("map").setView(L.latLng(41.79187262698525, -87.60107517242432), 14),
	july = [
		"08", "09", "10", 
		"11", "12", "13", 
		"14", "15", "16", 
		"17", "18", "19", 
		"20", "21", "22", 
		"23", "24", "25",
		"26", "27", "28",
		"29", "30", "31"
	],
	august = [
		"02"
	],
	url,
	i, l;

L.tileLayer.provider('OpenStreetMap.BlackAndWhite').addTo(map);

function httpGET(url, successHandler, errorHandler) {
	var xmlHttp = null;

	xmlHttp = new XMLHttpRequest();
	xmlHttp.open("GET", url);
	xmlHttp.onreadystatechange = function() {
		var status, data;

		if (xmlHttp.readyState == 4) {
			status = xmlHttp.status;

			if (status == 200) {
				data = JSON.parse(xmlHttp.responseText);
				successHandler && successHandler(data);
			} else {
				errorHandler && errorHandler(status);
			}
		}
	}
	xmlHttp.send();
}

function onSuccess(data) {
	L.geoJson(data, {
		style: { color: "#db1d0f", weight: 2 },
	}).addTo(map);
}

function onError(status) {
	console.log(status);
}

for (i = 0, l = july.length; i < l; i++) {
	url = "./data/tracks/2014-7-" + july[i] + ".geojson";
	httpGET(url, onSuccess, onError);
}

for (i = 0, l = august.length; i < l; i++) {
	url = "./data/tracks/2014-8-" + august[i] + ".geojson"
	httpGET(url, onSuccess, onError);
}