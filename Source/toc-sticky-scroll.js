$(document).ready(function () {  
  var bottom = $('.fluid-row').offset().top+$('.fluid-row').height();
  $(window).scroll(function (event) {
	var y = $(this).scrollTop();
		
		if (y >= 138) {
		  $('.tocify').css('top','-80px');
		} else {
		  $('.tocify').css('top',(58-y)+'px');
		}
	
  	});
});
