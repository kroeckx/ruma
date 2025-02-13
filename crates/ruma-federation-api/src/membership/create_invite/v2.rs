//! [PUT /_matrix/federation/v2/invite/{roomId}/{eventId}](https://matrix.org/docs/spec/server_server/r0.1.4#put-matrix-federation-v2-invite-roomid-eventid)

use ruma_api::ruma_api;
use ruma_events::{pdu::Pdu, AnyStrippedStateEvent};
use ruma_identifiers::{EventId, RoomId, RoomVersionId};
use ruma_serde::Raw;

ruma_api! {
    metadata: {
        description: "Invites a remote user to a room.",
        method: PUT,
        name: "create_invite",
        path: "/_matrix/federation/v2/invite/:room_id/:event_id",
        rate_limited: false,
        authentication: ServerSignatures,
    }

    request: {
        /// The room ID that the user is being invited to.
        #[ruma_api(path)]
        pub room_id: RoomId,

        /// The event ID for the invite event, generated by the inviting server.
        #[ruma_api(path)]
        pub event_id: EventId,

        /// The version of the room where the user is being invited to.
        pub room_version: RoomVersionId,

        /// The invite event which needs to be signed.
        pub event: Raw<Pdu>,

        /// An optional list of simplified events to help the receiver of the invite identify the room.
        pub invite_room_state: Vec<Raw<AnyStrippedStateEvent>>,
    }

    response: {
        /// The signed invite event.
        pub event: Raw<Pdu>,
    }
}

impl Request {
    /// Creates a new `Request` with the given room ID, event ID, room version, event and invite
    /// room state.
    pub fn new(
        room_id: RoomId,
        event_id: EventId,
        room_version: RoomVersionId,
        event: Raw<Pdu>,
        invite_room_state: Vec<Raw<AnyStrippedStateEvent>>,
    ) -> Self {
        Self { room_id, event_id, room_version, event, invite_room_state }
    }
}

impl Response {
    /// Creates a new `Response` with the given invite event.
    pub fn new(event: Raw<Pdu>) -> Self {
        Self { event }
    }
}
