import $ from 'jquery'
import fullcalendar from 'fullcalendar'
import Elm from "./User.elm";

var user = window.location.pathname.split( '/' ).slice(-1)[0].substring(1);

const app = Elm.Main.embed(document.getElementById('app'), {
  user: user
});

app.ports.events.subscribe(function(event) {
  console.log(event);
})

$(document).ready(function() {

    // page is now ready, initialize the calendar...

    $('#calendar').fullCalendar({
        dayClick: function(day, other) {
          console.log('a day has been clicked!', day, other);
        }
    })

});
