function changecss(){
$('.box-solid').addClass('test');
$('.test').css("box-shadow", "none");
$('.test').attr('style','box-shadow: 9px 9px 16px rgb(163,177,198,0.6), -9px -9px 16px  rgba(255,255,255, 0.5);');

// alert('ran!');
  }
  
  
 $(document).ready(function() {
   window.changecss();
 }
 );

// alert('Hello!');


