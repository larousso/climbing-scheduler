import $ from 'jquery'
import fullcalendar from 'fullcalendar'
import Elm from "./User.elm";

const app = Elm.Main.embed(document.getElementById('app'));

$(document).ready(function() {

    // page is now ready, initialize the calendar...

    $('#calendar').fullCalendar({
        // put your options and callbacks here
    })

});
