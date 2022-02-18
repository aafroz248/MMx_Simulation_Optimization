$(document).ready(function () {  
  var bottom = $('.fluid-row').offset().top+$('.fluid-row').height();
  $(window).scroll(function (event) {
	var y = $(this).scrollTop();
		if ((y-23) >= bottom) {
		  $('.tocify').css('top','-130px');
		} else {
		  $('.tocify').css('top',(bottom-y-110)+'px');
		}
	
  	});
});
