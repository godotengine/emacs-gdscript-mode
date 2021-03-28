;;; gdscript-keywords.el --- Cached keywords for the GDScript language -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 GDQuest

;; Author: Nathan Lovato <nathan@gdquest.com>
;; URL: https://github.com/godotengine/emacs-gdscript-mode/
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.3"))
;; Maintainer: nathan@gdquest.com
;; Created: Mar 2020
;; Keywords: languages

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;;  Cached keywords for the GDScript language, used for syntax highlighting and auto-completion.
;;
;;; Code:

(defconst gdscript-keywords '("and" "as" "assert" "break" "breakpoint" "case" "class" "class_name"
                              "const" "continue" "do" "elif" "else" "enum" "export" "extends" "false" "for" "func" "if" "in" "is"
                              "master" "match" "not" "onready" "or" "pass" "preload" "puppet" "remote" "remotesync" "return" "self" "setget" "signal"
                              "slave" "static" "switch" "sync" "tool" "true" "var" "while" "yield"))
(defconst gdscript-built-in-constants '("INF" "NAN" "PI" "TAU"))
;; Only contains types that are not classes and that the Godot editor highlights
;; like built-in keywords
(defconst gdscript-built-in-types '("bool" "float" "int" "null" "void"))
(defconst gdscript-built-in-functions '("Color8" "ColorN" "abs" "acos" "asin" "assert" "atan" "atan2"
                                        "bytes2var" "cartesian2polar" "ceil" "char" "clamp" "convert" "cos" "cosh" "db2linear" "decimals"
                                        "dectime" "deg2rad" "dict2inst" "ease" "exp" "floor" "fmod" "fposmod" "funcref" "get_stack" "hash"
                                        "inst2dict" "instance_from_id" "inverse_lerp" "is_equal_approx" "is_inf" "is_instance_valid" "is_nan"
                                        "is_zero_approx" "len" "lerp" "lerp_angle" "linear2db" "load" "log" "max" "min" "move_toward" "nearest_po2"
                                        "ord" "parse_json" "polar2cartesian" "posmod" "pow" "preload" "print" "print_debug" "print_stack" "printerr"
                                        "printraw" "prints" "printt" "push_error" "push_warning" "rad2deg" "rand_range" "rand_seed" "randf" "randi"
                                        "randomize" "range" "range_lerp" "round" "seed" "sign" "sin" "sinh" "smoothstep" "sqrt" "step_decimals" "stepify"
                                        "str" "str2var" "tan" "tanh" "to_json" "type_exists" "typeof" "validate_json" "var2bytes" "var2str" "weakref"
                                        "wrapf" "wrapi" "yield"))
;; Contains all engine classes and node types, including vectors, transforms, etc.
(defconst gdscript-built-in-classes '("AABB" "ARVRAnchor" "ARVRCamera" "ARVRController"
                                      "ARVRInterface" "ARVROrigin" "ARVRPositionalTracker" "ARVRServer" "AStar" "AStar2D" "AcceptDialog"
                                      "AnimatedSprite" "AnimatedSprite3D" "AnimatedTexture" "Animation" "AnimationNode"
                                      "AnimationNodeAdd2" "AnimationNodeAdd3" "AnimationNodeAnimation" "AnimationNodeBlend2"
                                      "AnimationNodeBlend3" "AnimationNodeBlendSpace1D" "AnimationNodeBlendSpace2D"
                                      "AnimationNodeBlendTree" "AnimationNodeOneShot" "AnimationNodeOutput" "AnimationNodeStateMachine"
                                      "AnimationNodeStateMachinePlayback" "AnimationNodeStateMachineTransition" "AnimationNodeTimeScale"
                                      "AnimationNodeTimeSeek" "AnimationNodeTransition" "AnimationPlayer" "AnimationRootNode"
                                      "AnimationTrackEditPlugin" "AnimationTree" "AnimationTreePlayer" "Area" "Area2D" "Array"
                                      "ArrayMesh" "AtlasTexture" "AudioBusLayout" "AudioEffect" "AudioEffectAmplify"
                                      "AudioEffectBandLimitFilter" "AudioEffectBandPassFilter" "AudioEffectChorus"
                                      "AudioEffectCompressor" "AudioEffectDelay" "AudioEffectDistortion" "AudioEffectEQ"
                                      "AudioEffectEQ10" "AudioEffectEQ21" "AudioEffectEQ6" "AudioEffectFilter"
                                      "AudioEffectHighPassFilter" "AudioEffectHighShelfFilter" "AudioEffectInstance"
                                      "AudioEffectLimiter" "AudioEffectLowPassFilter" "AudioEffectLowShelfFilter"
                                      "AudioEffectNotchFilter" "AudioEffectPanner" "AudioEffectPhaser" "AudioEffectPitchShift"
                                      "AudioEffectRecord" "AudioEffectReverb" "AudioEffectSpectrumAnalyzer"
                                      "AudioEffectSpectrumAnalyzerInstance" "AudioEffectStereoEnhance" "AudioServer" "AudioStream"
                                      "AudioStreamGenerator" "AudioStreamGeneratorPlayback" "AudioStreamMicrophone"
                                      "AudioStreamPlayback" "AudioStreamPlaybackResampled" "AudioStreamPlayer" "AudioStreamPlayer2D"
                                      "AudioStreamPlayer3D" "AudioStreamRandomPitch" "AudioStreamSample" "BackBufferCopy"
                                      "BakedLightmap" "BakedLightmapData" "BaseButton" "Basis" "BitMap" "BitmapFont" "Bone2D"
                                      "BoneAttachment" "BoxContainer" "BoxShape" "Button" "ButtonGroup" "CPUParticles" "CPUParticles2D"
                                      "Camera" "Camera2D" "CameraFeed" "CameraServer" "CameraTexture" "CanvasItem" "CanvasItemMaterial"
                                      "CanvasLayer" "CanvasModulate" "CapsuleMesh" "CapsuleShape" "CapsuleShape2D" "CenterContainer"
                                      "CharFXTransform" "CheckBox" "CheckButton" "CircleShape2D" "ClassDB" "ClippedCamera"
                                      "CollisionObject" "CollisionObject2D" "CollisionPolygon" "CollisionPolygon2D" "CollisionShape"
                                      "CollisionShape2D" "Color" "ColorPicker" "ColorPickerButton" "ColorRect" "ConcavePolygonShape"
                                      "ConcavePolygonShape2D" "ConeTwistJoint" "ConfigFile" "ConfirmationDialog" "Container" "Control"
                                      "ConvexPolygonShape" "ConvexPolygonShape2D" "Crypto" "CryptoKey" "CubeMap" "CubeMesh" "Curve"
                                      "Curve2D" "Curve3D" "CurveTexture" "CylinderMesh" "CylinderShape" "DampedSpringJoint2D"
                                      "Dictionary" "DirectionalLight" "Directory" "DynamicFont" "DynamicFontData" "EditorExportPlugin"
                                      "EditorFeatureProfile" "EditorFileDialog" "EditorFileSystem" "EditorFileSystemDirectory"
                                      "EditorImportPlugin" "EditorInspector" "EditorInspectorPlugin" "EditorInterface"
                                      "EditorNavigationMeshGenerator" "EditorPlugin" "EditorProperty" "EditorResourceConversionPlugin"
                                      "EditorResourcePreview" "EditorResourcePreviewGenerator" "EditorSceneImporter"
                                      "EditorSceneImporterAssimp" "EditorScenePostImport" "EditorScript" "EditorSelection"
                                      "EditorSettings" "EditorSpatialGizmo" "EditorSpatialGizmoPlugin" "EditorSpinSlider"
                                      "EditorVCSInterface" "EncodedObjectAsID" "Engine" "Environment" "Expression" "File" "FileDialog"
                                      "Font" "FuncRef" "GIProbe" "GIProbeData" "Generic6DOFJoint" "Geometry" "GeometryInstance"
                                      "Gradient" "GradientTexture" "GraphEdit" "GraphNode" "GridContainer" "GrooveJoint2D"
                                      "HBoxContainer" "HScrollBar" "HSeparator" "HSlider" "HSplitContainer" "HTTPClient" "HTTPRequest"
                                      "HashingContext" "HeightMapShape" "HingeJoint" "IP" "IP_Unix" "Image" "ImageTexture"
                                      "ImmediateGeometry" "Input" "InputDefault" "InputEvent" "InputEventAction" "InputEventGesture"
                                      "InputEventJoypadButton" "InputEventJoypadMotion" "InputEventKey" "InputEventMIDI"
                                      "InputEventMagnifyGesture" "InputEventMouse" "InputEventMouseButton" "InputEventMouseMotion"
                                      "InputEventPanGesture" "InputEventScreenDrag" "InputEventScreenTouch" "InputEventWithModifiers"
                                      "InputMap" "InstancePlaceholder" "InterpolatedCamera" "ItemList" "JSON" "JSONParseResult"
                                      "JSONRPC" "JavaScript" "Joint" "Joint2D" "KinematicBody" "KinematicBody2D" "KinematicCollision"
                                      "KinematicCollision2D" "Label" "LargeTexture" "Light" "Light2D" "LightOccluder2D" "Line2D"
                                      "LineEdit" "LineShape2D" "LinkButton" "Listener" "MainLoop" "MarginContainer" "Marshalls"
                                      "Material" "MenuButton" "Mesh" "MeshDataTool" "MeshInstance" "MeshInstance2D" "MeshLibrary"
                                      "MeshTexture" "MultiMesh" "MultiMeshInstance" "MultiMeshInstance2D" "MultiplayerAPI" "Mutex"
                                      "Navigation" "Navigation2D" "NavigationMesh" "NavigationMeshInstance" "NavigationPolygon"
                                      "NavigationPolygonInstance" "NetworkedMultiplayerPeer" "Nil" "NinePatchRect" "Node" "Node2D"
                                      "NodePath" "OS" "Object" "OccluderPolygon2D" "OmniLight" "OptionButton" "PCKPacker"
                                      "PHashTranslation" "PackedDataContainer" "PackedDataContainerRef" "PackedScene" "PacketPeer"
                                      "PacketPeerStream" "PacketPeerUDP" "Panel" "PanelContainer" "PanoramaSky" "ParallaxBackground"
                                      "ParallaxLayer" "Particles" "Particles2D" "ParticlesMaterial" "Path" "Path2D" "PathFollow"
                                      "PathFollow2D" "Performance" "PhysicalBone" "Physics2DDirectBodyState"
                                      "Physics2DDirectBodyStateSW" "Physics2DDirectSpaceState" "Physics2DServer" "Physics2DServerSW"
                                      "Physics2DShapeQueryParameters" "Physics2DShapeQueryResult" "Physics2DTestMotionResult"
                                      "PhysicsBody" "PhysicsBody2D" "PhysicsDirectBodyState" "PhysicsDirectSpaceState" "PhysicsMaterial"
                                      "PhysicsServer" "PhysicsShapeQueryParameters" "PhysicsShapeQueryResult" "PinJoint" "PinJoint2D"
                                      "Plane" "PlaneMesh" "PlaneShape" "PointMesh" "Polygon2D" "PolygonPathFinder" "PoolByteArray"
                                      "PoolColorArray" "PoolIntArray" "PoolRealArray" "PoolStringArray" "PoolVector2Array"
                                      "PoolVector3Array" "Popup" "PopupDialog" "PopupMenu" "PopupPanel" "Position2D" "Position3D"
                                      "PrimitiveMesh" "PrismMesh" "ProceduralSky" "ProgressBar" "ProjectSettings" "ProximityGroup"
                                      "ProxyTexture" "QuadMesh" "Quat" "RID" "RandomNumberGenerator" "Range" "RayCast" "RayCast2D"
                                      "RayShape" "RayShape2D" "Rect2" "RectangleShape2D" "Reference" "ReferenceRect" "ReflectionProbe"
                                      "RemoteTransform" "RemoteTransform2D" "Resource" "ResourceFormatLoader"
                                      "ResourceFormatLoaderCrypto" "ResourceFormatSaver" "ResourceFormatSaverCrypto" "ResourceImporter"
                                      "ResourceInteractiveLoader" "ResourceLoader" "ResourcePreloader" "ResourceSaver" "RichTextEffect"
                                      "RichTextLabel" "RigidBody" "RigidBody2D" "RootMotionView" "SceneState" "SceneTree"
                                      "SceneTreeTimer" "Script" "ScriptCreateDialog" "ScriptEditor" "ScrollBar" "ScrollContainer"
                                      "SegmentShape2D" "Semaphore" "Separator" "Shader" "ShaderMaterial" "Shape" "Shape2D" "ShortCut"
                                      "Skeleton" "Skeleton2D" "SkeletonIK" "Skin" "SkinReference" "Sky" "Slider" "SliderJoint"
                                      "SoftBody" "Spatial" "SpatialGizmo" "SpatialMaterial" "SpatialVelocityTracker" "SphereMesh"
                                      "SphereShape" "SpinBox" "SplitContainer" "SpotLight" "SpringArm" "Sprite" "Sprite3D"
                                      "SpriteBase3D" "SpriteFrames" "StaticBody" "StaticBody2D" "StreamPeer" "StreamPeerBuffer"
                                      "StreamPeerSSL" "StreamPeerTCP" "StreamTexture" "String" "StyleBox" "StyleBoxEmpty" "StyleBoxFlat"
                                      "StyleBoxLine" "StyleBoxTexture" "SurfaceTool" "TCP_Server" "TabContainer" "Tabs" "TextEdit"
                                      "TextFile" "Texture" "Texture3D" "TextureArray" "TextureButton" "TextureLayered" "TextureProgress"
                                      "TextureRect" "Theme" "Thread" "TileMap" "TileSet" "Timer" "ToolButton" "TouchScreenButton"
                                      "Transform" "Transform2D" "Translation" "TranslationServer" "Tree" "TreeItem" "TriangleMesh"
                                      "Tween" "UndoRedo" "VBoxContainer" "VScrollBar" "VSeparator" "VSlider" "VSplitContainer" "Variant"
                                      "Vector2" "Vector3" "VehicleBody" "VehicleWheel" "VideoPlayer" "VideoStream" "Viewport"
                                      "ViewportContainer" "ViewportTexture" "VisibilityEnabler" "VisibilityEnabler2D"
                                      "VisibilityNotifier" "VisibilityNotifier2D" "VisualInstance" "VisualServer" "VisualShader"
                                      "VisualShaderNode" "VisualShaderNodeBooleanConstant" "VisualShaderNodeBooleanUniform"
                                      "VisualShaderNodeColorConstant" "VisualShaderNodeColorFunc" "VisualShaderNodeColorOp"
                                      "VisualShaderNodeColorUniform" "VisualShaderNodeCompare" "VisualShaderNodeCubeMap"
                                      "VisualShaderNodeCubeMapUniform" "VisualShaderNodeCustom" "VisualShaderNodeDeterminant"
                                      "VisualShaderNodeDotProduct" "VisualShaderNodeExpression" "VisualShaderNodeFaceForward"
                                      "VisualShaderNodeFresnel" "VisualShaderNodeGlobalExpression" "VisualShaderNodeGroupBase"
                                      "VisualShaderNodeIf" "VisualShaderNodeInput" "VisualShaderNodeIs" "VisualShaderNodeOuterProduct"
                                      "VisualShaderNodeOutput" "VisualShaderNodeScalarClamp" "VisualShaderNodeScalarConstant"
                                      "VisualShaderNodeScalarDerivativeFunc" "VisualShaderNodeScalarFunc" "VisualShaderNodeScalarInterp"
                                      "VisualShaderNodeScalarOp" "VisualShaderNodeScalarSmoothStep" "VisualShaderNodeScalarSwitch"
                                      "VisualShaderNodeScalarUniform" "VisualShaderNodeSwitch" "VisualShaderNodeTexture"
                                      "VisualShaderNodeTextureUniform" "VisualShaderNodeTextureUniformTriplanar"
                                      "VisualShaderNodeTransformCompose" "VisualShaderNodeTransformConstant"
                                      "VisualShaderNodeTransformDecompose" "VisualShaderNodeTransformFunc"
                                      "VisualShaderNodeTransformMult" "VisualShaderNodeTransformUniform"
                                      "VisualShaderNodeTransformVecMult" "VisualShaderNodeUniform" "VisualShaderNodeVec3Constant"
                                      "VisualShaderNodeVec3Uniform" "VisualShaderNodeVectorClamp" "VisualShaderNodeVectorCompose"
                                      "VisualShaderNodeVectorDecompose" "VisualShaderNodeVectorDerivativeFunc"
                                      "VisualShaderNodeVectorDistance" "VisualShaderNodeVectorFunc" "VisualShaderNodeVectorInterp"
                                      "VisualShaderNodeVectorLen" "VisualShaderNodeVectorOp" "VisualShaderNodeVectorRefract"
                                      "VisualShaderNodeVectorScalarMix" "VisualShaderNodeVectorScalarSmoothStep"
                                      "VisualShaderNodeVectorScalarStep" "VisualShaderNodeVectorSmoothStep" "WeakRef" "WindowDialog"
                                      "World" "World2D" "WorldEnvironment" "X509Certificate" "XMLParser" "YSort"))

(provide 'gdscript-keywords)
;;; gdscript-keywords.el ends here
