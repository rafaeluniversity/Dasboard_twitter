
$(document).ready(function() {
	$("iframe[id^='tweet_']").load(function() {
		this.contentWindow.postMessage({ element: this.id,
			query: "height" }, "https://twitframe.com");
	});
});

/* listen for the return message once the tweet has been loaded */
$(window).bind("message", function(e) {
	var oe = e.originalEvent;

	if (oe.origin != "https://twitframe.com")
		return;

	if (oe.data.height && oe.data.element.match(/^tweet_/)){
	  console.log("oe");
		$("#" + oe.data.element).css("height", parseInt(oe.data.height) +10+ "px");
	}
});
