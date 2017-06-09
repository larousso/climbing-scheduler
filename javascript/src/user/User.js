import $ from 'jquery'
import fullcalendar from 'fullcalendar'
import moment from 'moment'
import AddEventMenu from "./AddEventMenu.elm";
import Elm from "./User.elm";

var user = window.location.pathname.split( '/' ).slice(-1)[0].substring(1);

const app = Elm.Main.embed(document.getElementById('app'), {
  user: user
});

app.ports.events.subscribe(function(event) {
  console.log(event);
})

$(document).ready(function() {
    var calendar = $('#calendar')
    calendar.fullCalendar({
        dayClick: function(day, event) {
          console.log('Event', event.target);
          $('#addEventMenu').remove();
          if ($(event.target).is('td')) {
            $(event.target).append('<div id="addEventMenu" style="position:relative;"></div>')
            const menu = AddEventMenu.Main.embed(document.getElementById('addEventMenu'), {
              date: day.format()
            });
            menu.ports.onDispose.subscribe(function(text) {
              console.log('OnDispose', text);
               $('#addEventMenu').remove();
            })
            app.ports.dayClicked.send([day.format(), event.pageX, event.pageY]);
          }
        }
    });
    app.ports.events.subscribe(function(event) {
      console.log(event);
      calendar.fullCalendar('addEventSource', event);
      calendar.fullCalendar('rerenderEvents' );
    })
});
//
// $('#calendar').contextmenu({
//     delegate: ".hasmenu",
//     preventContextMenuForPopup: true,
//     preventSelect: true,
//     menu: [{
//         title: "Edit Booking",
//         cmd: "Edit",
//         action: function(event, ui) {}
//     }, {
//         title: "Delete Booking",
//         cmd: "Delete",
//         action: function(event, ui) {
//             //need to pick up calEvent.ID here
//         }
//     }],
//     select: function(event, ui) {
//         // Logic for handing the selected option
//     },
//     beforeOpen: function(event, ui) {
//         ui.menu.zIndex($(event.target).zIndex() + 1);
//     },
// });
