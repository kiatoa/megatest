$(document).ready(function(){
    $(".viewmodal").click(function(){
        var modal =  document.getElementById("myModal" + this.id);
       // alert(this.id);
        modal.style.display = "block";
 
    });
    $(".close").click(function(){
        var modal =  document.getElementById("myModal" + this.id);
       // alert(this.id);
        modal.style.display = "none";
 
    });
});

