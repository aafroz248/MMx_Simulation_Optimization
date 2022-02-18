$(document).ready(function () {  
  var top = $('#colorGraphsPallete').offset().top;
  $(window).scroll(function (event) {
	var y = $(this).scrollTop();
		if (y >= 138) {
		  $('#colorGraphsPallete').addClass('fixedPalette');
		} else {
		  $('#colorGraphsPallete').removeClass('fixedPalette');
		}
	
  	});
});
